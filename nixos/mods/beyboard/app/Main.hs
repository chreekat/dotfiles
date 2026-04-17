{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
import Control.Exception (IOException, displayException, finally, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.ByteString.Char8 qualified as BS
import Data.IORef
import Data.List (find, isPrefixOf)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

import System.INotify (EventVariety (..), addWatch, initINotify)

import Evdev
import Evdev.Codes (Key (..))
import Evdev.Uinput (DeviceOpts (..), defaultDeviceOpts)
import Evdev.Uinput qualified as U

deviceName' :: BS.ByteString
deviceName' = BS.pack "beyboard-remap"

-- | Messages on the shared channel.
data Msg
    = DeviceEvent EventData
    | HoldTimeout Key Word

-- | Shared env used in all threads
data Env = Env
    { envSink :: U.Device
    , envChan :: Chan Msg
    , envDebug :: Bool
    , envCounter :: IORef Int
    , envGrabbed :: IORef (Set.Set String)
    }

-- | Tap/hold state machine, shared by all remapped keys.
data State
    = Idle
    | -- | Monotonic nanos when the key was pressed.
      Pending Word
    | Held
    deriving Eq

-- | A single tap/hold key remap.
data KeyRemap = KeyRemap
    { remapTrigger :: Key
    , remapHoldKey :: Key
    , remapTapKeys :: [(Key, KeyEvent)]
    , remapState :: IORef State
    , remapWake :: MVar Word
    , remapName :: Text
    }

holdTimeoutNs :: Word
holdTimeoutNs = 200_000_000

mkRemaps :: IO [KeyRemap]
mkRemaps = do
    capsRef <- newIORef Idle
    capsWake <- newEmptyMVar
    superRef <- newIORef Idle
    superWake <- newEmptyMVar
    pure
        [ KeyRemap
            { remapTrigger = KeyCapslock
            , remapHoldKey = KeyLeftctrl
            , remapTapKeys = [(KeyEsc, Pressed), (KeyEsc, Released)]
            , remapState = capsRef
            , remapWake = capsWake
            , remapName = "caps"
            }
        , KeyRemap
            { remapTrigger = KeyLeftmeta
            , remapHoldKey = KeyLeftmeta
            , remapTapKeys =
                [ (KeyLeftmeta, Pressed)
                , (KeyF5, Pressed)
                , (KeyF5, Released)
                , (KeyLeftmeta, Released)
                ]
            , remapState = superRef
            , remapWake = superWake
            , remapName = "super"
            }
        ]

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

    remaps <- mkRemaps
    let cleanup = forM_ remaps $ \r -> do
            st <- readIORef r.remapState
            when (st == Held) $ emitKey env r.remapHoldKey Released

    -- Start a timer thread for each remap.
    forM_ remaps $ \r -> forkIO $ remapTimer env r

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
    handleLoop env remaps `finally` cleanup

-- | Timer thread for a single remap. Blocks on the wake MVar until the
-- trigger key is pressed, sleeps for the hold timeout, then posts a
-- HoldTimeout message back through the main channel.
remapTimer :: Env -> KeyRemap -> IO ()
remapTimer env remap = forever $ do
    t0 <- takeMVar remap.remapWake
    now <- monoNanos
    let elapsed = now - t0
    when (elapsed < holdTimeoutNs) $
        threadDelay (fromIntegral ((holdTimeoutNs - elapsed) `div` 1_000))
    writeChan (envChan env) (HoldTimeout remap.remapTrigger t0)

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
        writeChan (envChan env) (DeviceEvent (eventData ev))
    case result of
        Left (e :: IOException) -> do
            atomicModifyIORef' (envGrabbed env) $ \s -> (Set.delete path s, ())
            logMsg env $ "Lost: " <> pack path
            logMsg env $ "Because we caught: " <> pack (displayException e)
        Right _ -> pure ()

-- | Single-threaded event handler. No timeout logic — timer threads handle
-- that and post HoldTimeout messages back through the channel.
handleLoop :: Env -> [KeyRemap] -> IO ()
handleLoop env remaps = forever $
    readChan (envChan env) >>= \case
        DeviceEvent ed -> handle env remaps ed
        HoldTimeout trigger t0 ->
            case find (\r -> r.remapTrigger == trigger) remaps of
                Just remap -> do
                    fired <- atomicModifyIORef' remap.remapState $ \case
                        Pending t | t == t0 -> (Held, True)
                        other -> (other, False)
                    when fired $ do
                        dbg env $ "  -> hold timeout, " <> remap.remapName <> " down"
                        emitKey env remap.remapHoldKey Pressed
                Nothing -> pure ()

handle :: Env -> [KeyRemap] -> EventData -> IO ()
handle env remaps ed = do
    case ed of
        KeyEvent key kev -> do
            tick env (pack (show key) <> " " <> pack (show kev))
            case find (\r -> r.remapTrigger == key) remaps of
                Just remap -> handleTrigger env remaps remap kev
                Nothing -> handleOther env remaps key kev ed
        _ -> U.writeEvent (envSink env) ed

handleTrigger :: Env -> [KeyRemap] -> KeyRemap -> KeyEvent -> IO ()
handleTrigger env remaps remap kev = do
    st <- readIORef remap.remapState
    case (st, kev) of
        (Idle, Pressed) -> do
            -- Pressing a trigger interrupts any other pending remaps.
            forM_ remaps $ \r ->
                when (r.remapTrigger /= remap.remapTrigger) $
                    transitionPendingToHeld env r
            dbg env $ remap.remapName <> " down (pending)"
            now <- monoNanos
            writeIORef remap.remapState (Pending now)
            void $ tryTakeMVar remap.remapWake
            putMVar remap.remapWake now
        (_, Repeated) ->
            pure ()
        (Pending _, Released) -> do
            dbg env $ "  -> " <> remap.remapName <> " tap"
            writeIORef remap.remapState Idle
            forM_ remap.remapTapKeys $ uncurry (emitKey env)
        (Held, Released) -> do
            dbg env $ "  -> " <> remap.remapName <> " up"
            writeIORef remap.remapState Idle
            emitKey env remap.remapHoldKey Released
        _ -> pure ()

handleOther :: Env -> [KeyRemap] -> Key -> KeyEvent -> EventData -> IO ()
handleOther env remaps key kev ed = do
    if kev == Pressed
        then do
            anyPending <- or <$> mapM (fmap isPending . readIORef . remapState) remaps
            if anyPending
                then do
                    forM_ remaps $ transitionPendingToHeld env
                    emitKey env key Pressed
                else U.writeEvent (envSink env) ed
        else U.writeEvent (envSink env) ed

-- | If a remap is in Pending state, transition it to Held and emit hold-key down.
transitionPendingToHeld :: Env -> KeyRemap -> IO ()
transitionPendingToHeld env r = do
    fired <- atomicModifyIORef' r.remapState $ \case
        Pending _ -> (Held, True)
        other -> (other, False)
    when fired $ do
        dbg env $ "  -> " <> r.remapName <> " held"
        emitKey env r.remapHoldKey Pressed

isPending :: State -> Bool
isPending (Pending _) = True
isPending _ = False

monoNanos :: IO Word
monoNanos = fromInteger . toNanoSecs <$> getTime Monotonic

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
