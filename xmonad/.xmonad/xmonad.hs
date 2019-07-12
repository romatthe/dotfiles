import Control.Monad
import Data.List

import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import qualified XMonad.DBus as D
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import Graphics.X11
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import System.IO
import XMonad.Actions.CycleWS

import Config.Keys
import Config.Options
import Hooks.Log
import Theme.Nord

main = do
  dbus <- D.connect
  D.requestAccess dbus
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ defaultConfig
      -- Simple stuff
      { terminal           = term options
      , focusFollowsMouse  = ffm options
      , modMask            = mask options
      , workspaces         = spaces options
      -- Theme
      , normalBorderColor  = unfocused theme
      , focusedBorderColor = focused theme
      , borderWidth        = border theme

      -- Lets hook up
      , handleEventHook    = eventHook
      --, logHook            = logHook' wsPanel
      --, logHook            = wsLogHook
      , logHook            = dynamicLogWithPP (myLogHook dbus)
      , layoutHook         = layoutHook'
      -- , manageHook         = manageHook'
      , manageHook         = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
      , startupHook        = starts options
      } `additionalKeysP` keybindings

---- Simple stuff
modm          = mod4Mask
mouseFocus    = True
keyboard      = myKeys

----- Appearance
bdrSize       = 5
bdrNormal     = bgColor
bdrFocus      = fgColor
font          = "Misc Termsyn.Icons:size=13"
monitorSize   = 1920
monitor n     = show(round(monitorSize * n))

----- WHAT COLOR?
bgColor       = "#383644"
fgColor       = "#FCEBDB"
wsBgColor     = "#B79288"
wsFgColor     = "#FFFFFF"
barBgColor    = "#383644"
barFgColor    = "#E2DED4"
hintColor     = "#AA3355"
layoutColor   = "#9D3E58"

---- Panel
leftBarSize   = monitor 0.7
rightBarSize  = monitor 0.3
leftBarPos    = "0"
rightBarPos   = leftBarSize
barHeight     = "26"

-- loghook
myLogHook dbus = def {
      ppOutput = D.send dbus
    , ppTitle = \i -> ""
    , ppSep = ""
    , ppWsSep = ""
    , ppCurrent = \i -> "%{T5}%{R} " ++ (w i) ++ "   %{R-}%{T-}"
    , ppHidden = \i -> " " ++ (w i) ++ " "
    , ppHiddenNoWindows = \i -> " " ++ (w i) ++ " "
    , ppVisibleNoWindows = Just (\i -> " " ++ (w i) ++ " ")
    , ppLayout = \l -> ""
    }
    where
        w "TERM" = "%{T4}\xf120%{T-}"
        w "INET" = "%{T4}\xf268%{T-}"
        w "DEV"  = "%{T4}\xf1cb%{T-}"
        w "ENT"  = "%{T4}\xf11b%{T-}"
        w "PLAY" = "%{T4}\xf001%{T-}"
        w "TOOL" = "%{T4}\xf0ad%{T-}"
        w i      = i

wsBar      =
  "dzen2 -dock -ta l      \
  \ -bg '" ++ barBgColor  ++ "' \
  \ -fg '" ++ barFgColor  ++ "' \
  \ -w  '" ++ leftBarSize ++ "' \
  \ -h  '" ++ barHeight   ++ "' \
  \ -x  '" ++ leftBarPos  ++ "' \
  \ -fn '" ++ font        ++ "' "

statsPanel      =
  "dzen2 -dock -ta r      \
  \ -bg '" ++ barBgColor   ++ "'\
  \ -fg '" ++ barFgColor   ++ "'\
  \ -w  '" ++ rightBarSize ++ "'\
  \ -h  '" ++ barHeight    ++ "'\
  \ -x  '" ++ rightBarPos  ++ "'\
  \ -fn '" ++ font         ++ "'"

statusConfig  = "conky -c ~/.xmonad/status.conf"
statsBar      = statusConfig ++ " | " ++ statsPanel

---- Hooks
eventHook     = fullscreenEventHook
layoutHook'   = myLayoutHook
logHook'      = myLogHook
manageHook'   = myManageHook

-- Layout Hook
myLayoutHook =
  avoidStruts
  $ smartBorders
  $ mkToggle (NOBORDERS ?? FULL ?? EOT)
  $ standardLayout
  where
    standardLayout =
      renamed [CutWordsLeft 2] $
      smartSpacingWithEdge 8 $ layoutHook defaultConfig

-- Manage Hook
myManageHook =
  composeAll . concat $
  [ [ className =? c --> doShift (w !! 1) | c <- inetApp ]
  , [ className =? c --> doShift (w !! 2) | c <- devApp ]
  , [ className =? c --> doShift (w !! 3) | c <- entApp ]
  , [ className =? c --> doShift (w !! 4) | c <- playApp ]
  , [ className =? c --> doFloat          | c <- floatingApp ]
  , [ className =? c --> doIgnore         | c <- ignoreApp ]
  , [ isDialog       --> doCenterFloat ]
  , [ isRole         --> doCenterFloat ]
  , [ manageDocks ]
  , [ manageHook def ]
  ]
  where
    w = spaces options
    isRole = stringProperty "WM_WINDOW_ROLE" =? "pop-up"
    inetApp = ["Chromium"]
    devApp =
      [ "SecureCRT", "GNS3", "VirtualBox Manager"
      , "VirtualBox Machine", "jetbrains-studio"
      , "Code", "oni"
      ]
    entApp = ["MPlayer", "smplayer", "mpv", "Gimp"]
    playApp = ["player", "Genymotion"]
    floatingApp = ["SecureCRT", "TeamViewer", "Xmessage"]
    ignoreApp = ["desktop", "desktop_window", "stalonetray", "trayer"]

-- Keymapping
myKeys =
  [ ((m, xK_b), spawn "google-chrome-stable")
  , ((m, xK_space), spawn "rofi -show drun -theme romatthe")
  , ((m, xK_Return), spawn $ term options)
  , ((m, xK_w), kill)
  , ((m, xK_f), sendMessage $ Toggle FULL)
  , ((m, xK_Right), windowGo R False)
  , ((m, xK_Left),  windowGo L False)
  , ((m, xK_Up),    windowGo U False)
  , ((m, xK_Down),  windowGo D False)
  , ((m, xK_s), sendMessage ToggleStruts)
  , ((m, xK_BackSpace), focusUrgent)
  , ((m, xK_equal), toggleWS)
  , ((m, xK_grave), toggleWS)
  , ((m .|. shiftMask, xK_t), sendMessage NextLayout)
  -- , ((m, xK_Caps_Lock), sendMessage $ Toggle FULL)
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
  , ((0, xK_Print), spawn "maim -s | xclip -selection clipboard -t image/png")
  , ((s, xK_Print), spawn "maim | xclip -selection clipboard -t image/png")
  ]
  where
    m = modm
    s = shiftMask
    c = controlMask
    dmenu =
      "dmenu_run -i \
      \ -fn '" ++ fn ++ "' \
      \ -w '" ++ w ++ "' \
      \ -h '" ++ h ++ "' \
      \ -x '" ++ x ++ "' \
      \ -nb '" ++ nb ++ "' \
      \ -sb '" ++ sb ++ "'"
      where
        w = monitor 0.3
        x = monitor 0.7
        h = "26"
        fn = font
        nb = bgColor
        sb = layoutColor
