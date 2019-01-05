{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           Data.Foldable   (msum)
import qualified Data.Map        as M
import           XMonad
import qualified XMonad.StackSet as W

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
    , terminal = "urxvtc -ls"
    , clickJustFocuses = True
    , focusFollowsMouse = False
    , layoutHook = MinOneMain (Tall 1 (3/100) (53/100)) ||| Full
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
