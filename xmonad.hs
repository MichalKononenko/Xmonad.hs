import XMonad
import XMonad.Config.Xfce

-- Set up workspaces
myWorkspaces = ["1:main", "2:web", "3:code", "4:testing", "5", "6", "7:email", "8:zotero"]
myManageHook = composeAll
    [
        className =? "Thunderbird" --> doShift "7:email"
    ,   className =? "Zotero" --> doShift "8:zotero"
    ]

main = xmonad xfceConfig
    {
        terminal = "xfce4-terminal"
    ,   manageHook = myManageHook <+> manageHook xfceConfig
    ,   modMask  = mod4Mask
    ,   workspaces = myWorkspaces
    ,   borderWidth = 2
    ,   normalBorderColor = "#d1d1d1"
    ,   focusedBorderColor = "#75e468"
    }

