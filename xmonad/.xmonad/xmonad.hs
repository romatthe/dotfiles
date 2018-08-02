import XMonad

import XMonad.Actions.Navigation2D

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

main :: IO ()
main = xmonad =<< xmobar myConfig

myXmobarCommand :: String
myXmobarCommand = "xmobar"

myXmobarPP = xmobarPP { ppCurrent = xmobarColor backgroundColor "" . wrap "<" ">" }

-- TODO: Get these colors from xrdb
backgroundColor   = "#FEFEFE"
middleColor       = "#AEAEAE"
foregroundColor   = "#0E0E0E"

myConfig = ewmh def
  { borderWidth        = 1
  , focusedBorderColor = foregroundColor
  , focusFollowsMouse  = False
  , handleEventHook    = fullscreenEventHook
  , layoutHook         = spacingWithEdge 10 emptyBSP ||| spacingWithEdge 10 (ThreeColMid 1 (3/100) (2/3))
  , modMask            = mod4Mask
  , normalBorderColor  = middleColor
  , terminal           = "tilix"
  , workspaces         = [ "browse", "code", "read", "chat", "etc" ]
  }
