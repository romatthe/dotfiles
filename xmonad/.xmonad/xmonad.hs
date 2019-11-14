import System.IO
import System.Exit

import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Navigation2D
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.Maximize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Font
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86

import Control.Monad (forM_, join)

import Data.Char
import Data.List (sortBy)
import Data.Function (on)
import Data.Bits ((.|.))
import qualified XMonad.StackSet as W
import qualified Data.Map as M

main :: IO ()
main = do
  --spawn xmproc
  xmonad $ ewmh $ defaults { handleEventHook = handleEventHook desktopConfig }

-- Border Styling
myBorderWidth = 2
myNormalBorderColor = "#BFBFBF"
myFocusedBorderColor = "#89DDFF"

-- State
myTerminal = "gnome-terminal"
-- CHANGE THIS!!!!!
myLauncher = "rofi -show drun"
myScreenshot = "shot" -- custom script '.local/bin/shot'

-- Set the MOD key to the Super key
modm = mod4Mask

-- Workspaces
workspaceNames =
  [ "Home"
  , "WWW"
  , "Code"
  , "Play"
  , "Misc"
  ]

xmproc = "polybar main" --statusbar

-- gaps (border / window spacing)
gaps = spacingRaw True (Border 0 0 0 0) False (Border 8 8 8 8) True
myWorkspaces = workspaceNames

-- Layout Hook
myLayout = maximize (ResizableTall 1 (3 / 100) (1 / 2) [] ||| Full)

-- myManageHook
myManageHook = composeAll . concat $
  [ [className =? "Emacs" --> doShift "Code"]
  , [className =? "Chrome" --> doShift "WWW"]
  ]

myNewManageHook = composeAll
  [ myManageHook
  , floatNextHook
  , manageHook desktopConfig
  ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $  [ ((modm, xK_Return)            , spawn myTerminal)
       , ((modm, xK_space)             , spawn myLauncher)
       , ((modm, xK_b)                 , spawn "google-chrome-stable")
       , ((modm, xK_w)                 , kill)
       , ((modm, xK_Tab)               , nextWS)
       , ((modm .|. shiftMask, xK_Tab) , prevWS)
       -- movement
       , ((modm, xK_Right), windowGo R False)
       , ((modm, xK_Left),  windowGo L False)
       , ((modm, xK_Up),    windowGo U False)
       , ((modm, xK_Down),  windowGo D False)
       -- toggle fullscreen (really just lower status bar below everything)
       , ((modm, xK_f), sendMessage ToggleStruts)
       , ((modm, xK_g), toggleWindowSpacingEnabled)
       -- audio keybindings
       , ((0, xF86XK_AudioRaiseVolume)   , spawn "amixer set Master 3+")
       , ((0, xF86XK_AudioLowerVolume)   , spawn "amixer set Master 3-")
       , ((0, xF86XK_AudioMute)          , spawn "amixer set Master toggle")
       ]

defaults =
  docks $ desktopConfig { borderWidth = myBorderWidth
                        , normalBorderColor  = myNormalBorderColor
                        , focusedBorderColor = myFocusedBorderColor
                        , focusFollowsMouse  = False
                        , modMask            = modm
                        , terminal           = myTerminal
                        , workspaces         = myWorkspaces
                        , keys               = myKeys
                        , manageHook         = myNewManageHook <+> manageDocks
                        , layoutHook         = avoidStruts $ gaps $ smartBorders $ myLayout
                        }
