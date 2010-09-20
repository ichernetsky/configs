import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome

main = xmonad gnomeConfig {
         terminal = "urxvt"
       , modMask = mod4Mask
       }
