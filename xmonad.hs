import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ShowWName
import XMonad.Layout.Spiral
import XMonad.Layout.Spacing
import XMonad.Hooks.SetWMName
import System.IO


-- Set up workspaces
myWorkspaces = ["1:main", "2:web", "3:code", "4:testing", "5:libreoffice", "6", "7:email", "8:zotero"]

myManageHook = composeAll
    [
        className =? "Thunderbird" --> doShift "7:email"
    ,   className =? "Zotero" --> doShift "8:zotero"
    ]

tiled = spacing 20 $ Tall nmaster delta ratio
    where
    -- number of windows in the master pane
    nmaster = 1
    -- Proportion of screen occupied by master pane
    ratio = 1/2
    -- Percent of screen to increment
    delta = 3/100

fibonacci_spiral = spacing 20 $ spiral(500/809)

myLayout = fibonacci_spiral ||| Full ||| tiled

main = do
    xmonad $ ewmh xfceConfig {
            terminal = "xfce4-terminal"
        ,   manageHook = myManageHook <+> manageHook xfceConfig
        ,   layoutHook = avoidStruts $ showWName myLayout
        ,   modMask  = mod4Mask
        ,   workspaces = myWorkspaces
        ,   borderWidth = 2
        ,   normalBorderColor = "#d1d1d1"
        ,   focusedBorderColor = "#75e468"
        ,   startupHook = setWMName "LG3D"
        ,   handleEventHook = handleEventHook xfceConfig <+> fullscreenEventHook
    } `additionalKeys` 
        [
            -- Toggle switch for muting the microphone
            ((0, 0x1008FFB2), spawn "amixer set Mic toggle")
            
            -- Brightness Controls
        ,   ((1, 0x1008FF11), spawn "xbacklight -dec 20")
        ,   ((1, 0x1008FF13), spawn "xbacklight -inc 20")
            -- Use the old "slock" button to start music
        ,   ((0, 0x1008FF41), spawn "mpc toggle")
        ]

