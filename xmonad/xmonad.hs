{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           Data.Foldable   (msum)
import qualified Data.Map        as M
import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Util.Paste (pasteString)
import           Data.Time (getCurrentTime, utctDay, showGregorian)

-- | A custom Tall that doesn't paradoxically allow zero windows in the main
-- space.
newtype MinOneMain a = MinOneMain (Tall a)
    deriving (Show, Read)

instance LayoutClass MinOneMain a where
    pureLayout (MinOneMain (Tall nmain _ frac)) r s = zip ws rs
        where
            ws = W.integrate s
            rs = tile frac r nmain (length ws)

    pureMessage (MinOneMain (Tall nmain delta frac)) m =
        msum
            [ fmap (MinOneMain . resize) (fromMessage m)
            , fmap (MinOneMain . incMain) (fromMessage m)]

        where
            resize Shrink = Tall nmain delta (max (1/100) $ frac-delta)
            resize Expand = Tall nmain delta (min (99/100) $ frac+delta)
            incMain (IncMasterN d) = Tall (max 1 (nmain+d)) delta frac

    description _ = "Paradox-free Tall"

main = xmonad defaultConfig
    { modMask = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "#262626"
    , normalBorderColor = "#bcbcbc"
    , clickJustFocuses = True
    , focusFollowsMouse = False
    , keys = myKeys
    , layoutHook = MinOneMain (Tall 1 (3/100) (53/100)) ||| Full ||| Mirror (MinOneMain (Tall 1 (3/100) (1/3)))
    , terminal = "urxvtc -ls"
    }

-- | Discovered with xev
xK_XF86MonBrightnessUp, xK_XF86MonBrightnessDown :: KeySym
xK_XF86MonBrightnessUp = 0x1008ff02
xK_XF86MonBrightnessDown = 0x1008ff03

myKeys conf@(XConfig { modMask = modm }) =
  keyList `M.union` (keys defaultConfig conf)
 where
  keyList = M.fromList
    (  [ ( (0, xK_XF86MonBrightnessUp)
         , spawn "/home/b/.xmonad/chg_intel_brightness.sh up"
         )
       , ( (0, xK_XF86MonBrightnessDown)
         , spawn "/home/b/.xmonad/chg_intel_brightness.sh down"
         )
       , ( (modm, xK_F9)
         , (pasteString . showGregorian . utctDay) =<< (liftIO getCurrentTime)
         )
       , ( (modm, xK_a)
         ,
           -- screensaver
           spawn "xset s activate"
         )
       , ( (0, xK_Print)
         , spawn "flameshot gui"
         )
       -- # Dvorak fuckery
       -- shift-c was stolen
       -- kill window
       , ( (modm, xK_F4)
         , kill
         )
        -- mod-t was stolen
        -- sink floating window into tiling
       , ( (modm, xK_s)
         , withFocused (windows . W.sink)
         )
       -- modm-j and -k are real finger twisters
       -- modify the window order
       , ((modm .|. shiftMask, xK_h), windows W.swapDown)
       , ((modm .|. shiftMask, xK_t), windows W.swapUp)
       , ((modm, xK_h)              , windows W.focusDown)
       , ( (modm, xK_t)
         , windows W.focusUp
         )
       -- Now need to have resize too.
       , ((modm, xK_d), sendMessage Shrink)
       , ((modm, xK_l), sendMessage Expand)
       ]
    -- Xinerama screens with dvorak-friendlier bindings: g, c, r
    ++ [ ( (modm .|. m, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_g, xK_c, xK_r] [0 ..]
       , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
       ]
    )

-- cheat sheet from XMonad sources.
{-

keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $
    -- launching and killing programs
       [ ( (modMask .|. shiftMask, xK_Return)
         , spawn $ XMonad.terminal conf
         ) -- %! Launch terminal
       , ( (modMask, xK_p)
         , spawn "dmenu_run"
         ) -- %! Launch dmenu
       , ( (modMask .|. shiftMask, xK_p)
         , spawn "gmrun"
         ) -- %! Launch gmrun
       , ( (modMask .|. shiftMask, xK_c)
         , kill
         ) -- %! Close the focused window
       , ( (modMask, xK_space)
         , sendMessage NextLayout
         ) -- %! Rotate through the available layout algorithms
       , ( (modMask .|. shiftMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         ) -- %!  Reset the layouts on the current workspace to default
       , ( (modMask, xK_n)
         , refresh
         ) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
       , ( (modMask, xK_Tab)
         , windows W.focusDown
         ) -- %! Move focus to the next window
       , ( (modMask .|. shiftMask, xK_Tab)
         , windows W.focusUp
         ) -- %! Move focus to the previous window
       , ( (modMask, xK_j)
         , windows W.focusDown
         ) -- %! Move focus to the next window
       , ( (modMask, xK_k)
         , windows W.focusUp
         ) -- %! Move focus to the previous window
       , ( (modMask, xK_m)
         , windows W.focusMaster
         ) -- %! Move focus to the master window

    -- modifying the window order
       , ( (modMask, xK_Return)
         , windows W.swapMaster
         ) -- %! Swap the focused window and the master window
       , ( (modMask .|. shiftMask, xK_j)
         , windows W.swapDown
         ) -- %! Swap the focused window with the next window
       , ( (modMask .|. shiftMask, xK_k)
         , windows W.swapUp
         ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
       , ( (modMask, xK_h)
         , sendMessage Shrink
         ) -- %! Shrink the master area
       , ( (modMask, xK_l)
         , sendMessage Expand
         ) -- %! Expand the master area

    -- floating layer support
       , ( (modMask, xK_t)
         , withFocused $ windows . W.sink
         ) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
       , ( (modMask, xK_comma)
         , sendMessage (IncMasterN 1)
         ) -- %! Increment the number of windows in the master area
       , ( (modMask, xK_period)
         , sendMessage (IncMasterN (-1))
         ) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
       , ( (modMask .|. shiftMask, xK_q)
         , io (exitWith ExitSuccess)
         ) -- %! Quit xmonad
       , ( (modMask, xK_q)
         , spawn
           "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
         ) -- %! Restart xmonad
       , ( (modMask .|. shiftMask, xK_slash)
         , helpCommand
         ) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
    -- repeat the binding for non-American layout keyboards
       , ((modMask, xK_question), helpCommand) -- %! Run xmessage with a summary of the default keybindings (useful for beginners)
       ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
       [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
       [ ( (m .|. modMask, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       ]
 where
  helpCommand :: X ()
  helpCommand = spawn ("echo " ++ show help ++ " | xmessage -file -")

-- | Mouse bindings: default actions bound to mouse events
mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings (XConfig { XMonad.modMask = modMask }) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
  [ ( (modMask, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )
    -- mod-button2 %! Raise the window to the top of the stack
  , ( (modMask, button2)
    , windows . (W.shiftMaster .) . W.focusWindow
    )
    -- mod-button3 %! Set the window to floating mode and resize by dragging
  , ( (modMask, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

-}
