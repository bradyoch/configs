import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

main = do
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ desktopConfig
    { terminal      = myTerminal
    , modMask       = myModMask
    , keys          = myKeys <+> keys def
    , mouseBindings = myMouseBindings <+> mouseBindings def
    , borderWidth   = myBorderWidth
    , logHook       = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle  = xmobarColor "green" ""
                    , ppSep    = " | "
                    }
    , layoutHook    = spacingRaw False (Border 0 5 5 5) True (Border 10 10 10 10) True $
                    avoidStruts $ layoutHook def
    , startupHook   = setWMName "LG3D"
    , manageHook    = manageDocks <+> manageHook defaultConfig
    }

myTerminal    = "kitty"
myModMask     = mod4Mask
myBorderWidth = 2
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm, xK_space), spawn "rofi -show run")
  , ((0, xF86XK_AudioMute), spawn "pamixer -t")
  , ((0, xF86XK_AudioRaiseVolume), spawn "pamixer -u -i 5")
  , ((0, xF86XK_AudioLowerVolume), spawn "pamixer -d 5")
  , ((0, xF86XK_MonBrightnessUp), spawn "light -A 5")
  , ((0, xF86XK_MonBrightnessDown), spawn "light -U 5")
  , ((modm, xK_Return), spawn $ XMonad.terminal conf)
  , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
  , ((modm, xK_b), sendMessage NextLayout)
  , ((modm, xK_f), toggleSmartSpacing)
  ]
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w
                                                   >> windows W.shiftMaster))
  ]
