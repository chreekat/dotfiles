import XMonad
import qualified Data.Map as M

main = xmonad defaultConfig
    { modMask = mod4Mask
    , terminal = "urxvt"
    , keys = myKeys
    }

xK_XF86MonBrightnessUp, xK_XF86MonBrightnessDown :: KeySym
xK_XF86MonBrightnessUp = 0x1008ff02
xK_XF86MonBrightnessDown = 0x1008ff03

myKeys conf@(XConfig {modMask = modm}) =
    keyList `M.union` (keys defaultConfig conf)
  where
    keyList = M.fromList
        [ ((0, xK_XF86MonBrightnessUp),
            spawn "/home/b/.xmonad/chg_intel_brightness.sh up")
        , ((0, xK_XF86MonBrightnessDown),
            spawn "/home/b/.xmonad/chg_intel_brightness.sh down")
        , ((modm, xK_a),
            -- screensaver
            spawn "xset s activate")
        ]
