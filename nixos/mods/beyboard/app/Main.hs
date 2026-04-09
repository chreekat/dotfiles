{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (IOException, displayException, finally, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.ByteString.Char8 qualified as BS
import Data.IORef
import Data.List (isPrefixOf)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import System.Timeout (timeout)

import System.INotify (EventVariety (..), addWatch, initINotify)

import Evdev
import Evdev.Codes (Key (..))
import Evdev.Uinput (DeviceOpts (..), defaultDeviceOpts)
import Evdev.Uinput qualified as U

deviceName' :: BS.ByteString
deviceName' = BS.pack "capslock-remap"

-- | Shared env used in all threads
data Env = Env
    { envSink :: U.Device
    , envChan :: Chan EventData
    , envDebug :: Bool
    , envCounter :: IORef Int
    , envGrabbed :: IORef (Set.Set String)
    }

-- | Capslock state machine.
data State
    = Idle
    | -- | Monotonic nanos when capslock was pressed.
      Pending Word
    | CtrlHeld
    deriving Eq

holdTimeoutNs :: Word
holdTimeoutNs = 200_000_000

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    debug <-
        getArgs >>= \case
            ["--debug"] -> pure True
            [] -> pure False
            _ -> error "Usage: beyboard [--debug]"

    let opts = defaultDeviceOpts{keys = [minBound .. maxBound]}
    sink <- U.newDevice deviceName' opts

    chan <- newChan
    ctr <- newIORef (0 :: Int)
    -- We consider ourself grabbed. Hey now
    self <-
        maybe (fail "Could not get devnode for sink device") (pure . BS.unpack)
            =<< U.deviceDevnode sink
    grabbed <- newIORef (Set.singleton self)
    let env = Env sink chan debug ctr grabbed

    ref <- newIORef Idle
    let cleanup = do
            st <- readIORef ref
            when (st == CtrlHeld) $ emitKey env KeyLeftctrl Released

    dbg env "Initial scan for keyboards..."
    scanAndGrab env

    -- Watch for new devices via inotify.
    ino <- initINotify
    void $ addWatch ino [Create] (BS.pack "/dev/input") $ \_ -> do
        dbg env "Device change detected, rescanning for keyboards..."
        -- Let the device get its permissions and stuff
        threadDelay 500_000
        scanAndGrab env

    -- Single handler thread processes all events.
    -- FIXME: Not sure we need the cleanup?
    handleLoop env ref `finally` cleanup

-- | One-shot scan and grab of all keyboards. Skips ones we already grabbed.
scanAndGrab :: Env -> IO ()
scanAndGrab env = do
    files <- listDirectory "/dev/input"
    let eventFiles = filter ("event" `isPrefixOf`) files
    alreadyGrabbed <- readIORef (envGrabbed env)
    forM_ eventFiles $ \f -> do
        let path = "/dev/input/" ++ f
        unless (path `Set.member` alreadyGrabbed) $ void $ forkIO $ do
            result <- try $ manageKeyboard env path
            case result of
                Left (_ :: IOException) -> pure ()
                Right _ -> pure ()

-- | Grab the keyboard and start processing its input in a new thread
manageKeyboard :: Env -> String -> IO ()
manageKeyboard env path = do
    dev <- newDevice (BS.pack path)
    isKbd <- deviceHasEvent dev (KeyEvent KeyA Pressed)
    when isKbd $ do
        grabDevice dev
        atomicModifyIORef' (envGrabbed env) $ \s -> (Set.insert path s, ())
        name <- deviceName dev
        logMsg env $ "Grabbed: " <> decodeUtf8 name <> " (" <> pack path <> ")"
        _ <- forkIO $ deviceLoop env dev path
        pure ()

-- | Read events from a device and forward them to the shared channel.
deviceLoop :: Env -> Device -> String -> IO ()
deviceLoop env dev path = do
    result <- try $ forever $ do
        ev <- nextEvent dev
        writeChan (envChan env) (eventData ev)
    case result of
        Left (e :: IOException) -> do
            atomicModifyIORef' (envGrabbed env) $ \s -> (Set.delete path s, ())
            logMsg env $ "Lost: " <> pack path
            logMsg env $ "Because we caught: " <> pack (displayException e)
        Right _ -> pure ()

-- | Single-threaded event handler
handleLoop :: Env -> IORef State -> IO ()
handleLoop env ref =
    forever $
        readIORef ref >>= \case
            Pending t0 -> do
                now <- fromInteger . toNanoSecs <$> getTime Monotonic
                if now - t0 >= holdTimeoutNs
                    then do
                        emitKey env KeyLeftctrl Pressed
                        writeIORef ref CtrlHeld
                    else do
                        let remainUs = fromIntegral ((holdTimeoutNs - (now - t0)) `div` 1_000)
                        timeout (max 1 remainUs) (readChan (envChan env)) >>= \case
                            Nothing -> do
                                dbg env "  -> hold timeout, ctrl down"
                                emitKey env KeyLeftctrl Pressed
                                writeIORef ref CtrlHeld
                            Just ed -> handle env ref ed
            _ -> readChan (envChan env) >>= handle env ref

handle :: Env -> IORef State -> EventData -> IO ()
handle env ref ed =
    readIORef ref >>= \st -> do
        case ed of
            KeyEvent key kev ->
                tick env (showState st <> " | " <> pack (show key) <> " " <> pack (show kev))
            _ -> pure ()
        case (st, ed) of
            (Idle, KeyEvent KeyCapslock Pressed) -> do
                dbg env "caps down (pending)"
                now <- fromInteger . toNanoSecs <$> getTime Monotonic
                writeIORef ref (Pending now)
            (_, KeyEvent KeyCapslock Repeated) ->
                pure ()
            (Pending _, KeyEvent key Pressed) | key /= KeyCapslock -> do
                dbg env "  -> ctrl+key"
                emitKey env KeyLeftctrl Pressed
                emitKey env key Pressed
                writeIORef ref CtrlHeld
            (Pending _, KeyEvent KeyCapslock Released) -> do
                dbg env "  -> esc tap"
                emitKey env KeyEsc Pressed
                emitKey env KeyEsc Released
                writeIORef ref Idle
            (CtrlHeld, KeyEvent KeyCapslock Released) -> do
                dbg env "  -> ctrl up"
                emitKey env KeyLeftctrl Released
                writeIORef ref Idle
            _ -> U.writeEvent (envSink env) ed

showState :: State -> Text
showState Idle = "IDLE"
showState (Pending _) = "PEND"
showState CtrlHeld = "CTRL"

emitKey :: Env -> Key -> KeyEvent -> IO ()
emitKey env key kev = U.writeBatch (envSink env) [KeyEvent key kev]

-- | Log a key event every 20th time to avoid leaking keystrokes.
tick :: Env -> Text -> IO ()
tick env msg = when (envDebug env) $ do
    n <- readIORef (envCounter env)
    let n' = (n + 1) `mod` 20
    writeIORef (envCounter env) n'
    when (n' == 0) $ dbg env msg

dbg :: Env -> Text -> IO ()
dbg env msg = when (envDebug env) $ BS.hPutStrLn stderr (encodeUtf8 msg)

logMsg :: Env -> Text -> IO ()
logMsg _env msg = BS.hPutStrLn stdout (encodeUtf8 msg)
