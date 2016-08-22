import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.ShowWName
import XMonad.Layout.Spiral
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Set up workspace
myWorkspaces = ["1:main", "2:web", "3:code", "4:testing", "5", "6", "7:email", "8:zotero"]
myManageHook = composeAll 
    [ className =? "thunderbird" --> doShift "7:email"
    , className =? "firefox" --> doShift "2:web"
    , className =? "zotero" --> doShift "8:zotero"
    ]

-- Define the tiling algorithm
fullscreen = Full

tiled = Tall nmaster delta ratio
    where
    -- Number of windows in the master pane
    nmaster = 1

    -- Proportion of screen occupied by master pane
    ratio = 1/2

    -- Percent of screen to increment
    delta = 3/100

fibonacci_spiral = spiral(500/809)


myLayout = fullscreen ||| tiled ||| fibonacci_spiral

main = do
    -- Spawn the status bar at the top of the screen using the xmobarrc in this directory
    xmproc <- spawnPipe "/usr/bin/xmobar /home/mkononen/.xmonad/xmobar.hs" 
    
    xmonad $ defaultConfig {
        manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig 
      , layoutHook = avoidStruts $ showWName  myLayout
      , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "#657b83" "" . shorten 0
            , ppLayout = const ""
            }
      , borderWidth = 2
      , normalBorderColor = "#d1d1d1"
      , focusedBorderColor = "#75e468"
      , workspaces = myWorkspaces
      , terminal = "urxvt"
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
        
          -- Spawn the menu with appropriate parameters
        , ((mod1Mask, xK_p), spawn "dmenu_run -fn \
            \ \"xft:Roboto Regular:size=9:antialias=true\" \
            \ -nb \"#000000\" -nf \"#657b83\" \
            \ -sb \"yellow\" -sf \"#000000\" "
          )
        ]

