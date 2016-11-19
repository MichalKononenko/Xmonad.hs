import XMonad
import XMonad.Config.Xfce

main = xmonad xfceConfig
    {
        terminal = "urxvt"
    ,   modMask  = mod4Mask
    }
