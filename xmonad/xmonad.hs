{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.WindowProperties

import qualified XMonad.StackSet as W

import Control.Monad
import Data.Ratio ((%))

main = xmonad $ gnomeConfig {
         terminal = myTerminal
       , modMask = mod4Mask
       , workspaces = myWorkspaces
       , layoutHook = myLayoutHook
       , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
       , startupHook = startupHook gnomeConfig >> setWMName "LG3D"
       }
       `additionalKeysP`
       myKeymap

myTerminal :: String
myTerminal = "urxvt"

myWorkspaces :: [String]
myWorkspaces = [ "web", "dev", "term", "im", "office" ]

myLayoutHook = smartBorders $
    onWorkspace "im" imLayout
    $ defaultLayout


defaultLayout = makeLayout $ Full ||| tall ||| Mirror tall
imLayout = makeLayout $ reflectHoriz $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1 % 6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = (ClassName "Pidgin") `And` (Role "buddy_list")
    skypeRoster     = (Title "ivan.chernetsky - Skype™ (Beta)") `Or` (Title "Skype™ 2.1 (Beta) for Linux")
makeLayout layout = desktopLayoutModifiers layout
tall = Tall 1 0.03 0.5

data AddRosters a = AddRosters Rational [Property]
                    deriving (Read, Show)

instance LayoutModifier AddRosters Window where
    modifyLayout (AddRosters ratio props) = applyIMs ratio props
    modifierDescription _                 = "IMs"

withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props

applyIMs :: (LayoutClass l Window) =>
           Rational
         -> [Property]
         -> W.Workspace WorkspaceId (l Window) Window
         -> Rectangle
         -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
  let stack = W.stack wksp
  let ws = W.integrate' $ stack
  rosters <- filterM (hasAnyProperty props) ws
  let n = fromIntegral $ length rosters
  let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
  let rosterRects = splitHorizontally n rostersRect
  let filteredStack = stack >>= W.filter (`notElem` rosters)
  (a,b) <- runLayout (wksp {W.stack = filteredStack}) chatsRect
  return (zip rosters rosterRects ++ a, b)

hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
  b <- hasProperty p w
  if b then return True else hasAnyProperty ps w

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
