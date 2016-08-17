import XMonad
import XMonad.Layout.ShowWName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    spawn "xmobar &" 
    xmonad $ defaultConfig {
        manageHook = manageHook defaultConfig <+> manageDocks
      , layoutHook = avoidStruts $ showWName (layoutHook defaultConfig)
    } `additionalKeys` [
	  ((0, 0x1008FF12), spawn "amixer set Master toggle")
        , ((0, 0x1008FF11), spawn "amixer set Master 2-")
        , ((0, 0x1008FF13), spawn "amixer set Master 2+")
        , ((0, 0x1008FFB2), spawn "amixer set Mic toggle")
        , ((0, 0x1008FF41), spawn "slock")
        , ((1, 0x1008FF11), spawn "xbacklight -dec 20")
        , ((1, 0x1008FF13), spawn "xbacklight -inc 20")]

