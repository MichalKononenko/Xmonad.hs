import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.ShowWName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Set up workspace
myWorkspaces = ["1:main", "2:web", "3:code", "4:testing", "5", "6", "7", "8:email"]
myManageHook = composeAll 
    [ className =? "thunderbird" --> doShift "8:email"
    , className =? "firefox" --> doShift "2:web"
    ]

main = do
    -- Spawn the status bar at the top of the screen using the xmobarrc in this directory
    xmproc <- spawnPipe "/usr/bin/xmobar /home/mkononen/.xmonad/xmobarrc" 
    
    xmonad $ defaultConfig {
        manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig 
      , layoutHook = avoidStruts $ showWName (layoutHook defaultConfig)
      , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "#657b83" "" . shorten 0
            , ppLayout = const ""
            }
      , borderWidth = 2
      , normalBorderColor = "#d1d1d1"
      , focusedBorderColor = "#75e468"
      , workspaces = myWorkspaces
    } `additionalKeys` 
        [ -- Volume mute key
	      ((0, 0x1008FF12), spawn "amixer set Master toggle")
          
          -- Volume control
        , ((0, 0x1008FF11), spawn "amixer set Master 2-")
        , ((0, 0x1008FF13), spawn "amixer set Master 2+")
         
          -- Mic mute key
        , ((0, 0x1008FFB2), spawn "amixer set Mic toggle")
          
          -- Key beside mic mute button, use for screen locker
        , ((0, 0x1008FF41), spawn "slock")
          
          -- Let shift + volume keys control the brightness level 
        , ((1, 0x1008FF11), spawn "xbacklight -dec 20")
        , ((1, 0x1008FF13), spawn "xbacklight -inc 20")
        ]

