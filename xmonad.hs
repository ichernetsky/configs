import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as W

import Data.Ratio ((%))

main = xmonad $ gnomeConfig {
         terminal = myTerminal
       , modMask = mod4Mask
       , workspaces = myWorkspaces
       , layoutHook = myLayoutHook
       , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
       }
       `additionalKeysP`
       myKeymap

myTerminal :: String
myTerminal = "urxvt"

myWorkspaces :: [String]
myWorkspaces = [ "web", "dev", "term", "im", "office" ]

myLayoutHook =
    onWorkspace "im" imLayout
    $ defaultLayout


defaultLayout = makeLayout $ Full ||| tall ||| Mirror tall
imLayout = makeLayout $
           makeIMLayout (And (ClassName "Pidgin")
                             (Role "buddy_list"))
makeIMLayout property = withIM (1 % 5) property Full
makeLayout layout = desktopLayoutModifiers layout
tall = Tall 1 0.03 0.5

myXPConfig = defaultXPConfig {
	       font = "-xos4-terminus-*-*-*-*-14-*-*-*-*-*-iso10646-1"
             , promptBorderWidth = 0
	     , fgColor = "#00ffff"
	     , bgColor = "#000000"
	     , bgHLight = "#000000"
	     , fgHLight = "#ff0000"
	     , position = Top
             }

myKeymap =
    [ ("M-s", scratchpadSpawnActionTerminal "urxvt")
    , ("M-x", shellPrompt myXPConfig) ]

myManageHook =
    [ scratchpadManageHook (W.RationalRect 0.325 0.6 0.641 0.35)
    , web "Chrome"
    , web "Firefox"
    , dev "Emacs"
    , im "Pidgin"
    , im "Skype"
    , office "Thunderbird"
    , office "OpenOffice.org 3.2" ]

web = associate "web"
dev = associate "dev"
term = associate "term"
im = associate "im"
office = associate "office"
associate area wmClass = className =? wmClass --> doShift area
