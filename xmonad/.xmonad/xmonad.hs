import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.BinarySpacePartition
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import XMonad.Util.CustomKeys


main :: IO ()
main =
  xmonad
    $ withNavigation2DConfig def { defaultTiledNavigation = hybridNavigation }
    $ myConfig


-- TODO: Get these colors from xrdb
backgroundColor   = "#FFFFFF"
middleColor       = "#AEAEAE"
foregroundColor   = "#000000"

myConfig = ewmh def
  { borderWidth        = 1
  , focusedBorderColor = foregroundColor
  , focusFollowsMouse  = False
  , handleEventHook    = fullscreenEventHook
  , keys               = myKeys
  , layoutHook         =   smartBorders $ smartSpacingWithEdge 1 emptyBSP
                       ||| smartSpacingWithEdge 1 (ThreeColMid 1 (3/100) (2/3))
  , modMask            = mod4Mask
  , normalBorderColor  = middleColor
  , terminal           = "gnome-terminal"
  , workspaces         = [ "browse", "code", "read", "chat", "etc" ]
  }

myKeys = customKeys removedKeys addedKeys

removedKeys :: XConfig l -> [(KeyMask, KeySym)]
removedKeys XConfig {modMask = modm} =
    [ (modm              , xK_space)  -- Default for layout switching
    , (modm .|. shiftMask, xK_Return) -- Default for opening a terminal
    , (modm .|. shiftMask, xK_c)      -- Default for closing the focused window
    ]

addedKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addedKeys conf @ XConfig {modMask = modm} =
  [ -- Application launcher
    ((modm, xK_space) , spawn "rofi -show drun -theme romatthe")

    -- Terminal
  , ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- Close application
  , ((modm, xK_w), kill)

    -- Switch to last workspace
  , ((modm, xK_Tab), toggleWS)

    -- Rotate windows
  , ((modm, xK_r), sendMessage Rotate)

    -- Swap windows
  , ((modm, xK_t), sendMessage Swap)

    -- Layout switching
  , ((modm .|. shiftMask, xK_t), sendMessage NextLayout)

    -- Directional navigation of windows
  , ((modm, xK_l), windowGo R False)
  , ((modm, xK_h), windowGo L False)
  , ((modm, xK_k), windowGo U False)
  , ((modm, xK_j), windowGo D False)

    -- Expand and shrink windows
  , ((modm .|. controlMask,                xK_l), sendMessage $ ExpandTowards R)
  , ((modm .|. controlMask,                xK_h), sendMessage $ ExpandTowards L)
  , ((modm .|. controlMask,                xK_j), sendMessage $ ExpandTowards D)
  , ((modm .|. controlMask,                xK_k), sendMessage $ ExpandTowards U)
  , ((modm .|. controlMask .|. shiftMask , xK_l), sendMessage $ ShrinkFrom R)
  , ((modm .|. controlMask .|. shiftMask , xK_h), sendMessage $ ShrinkFrom L)
  , ((modm .|. controlMask .|. shiftMask , xK_j), sendMessage $ ShrinkFrom D)
  , ((modm .|. controlMask .|. shiftMask , xK_k), sendMessage $ ShrinkFrom U)

    -- Expand and shrink floating windows
  , ((modm .|. controlMask, xK_n), sendMessage Expand)
  , ((modm .|. controlMask, xK_m), sendMessage Shrink)

    -- Brightness control
  , ((0,         0x1008ff41), spawn "sudo /home/rleppink/.local/bin/tpb -t")
  , ((modm,         xK_F3), spawn "sudo /home/rleppink/.local/bin/tpb -t")
  , ((modm,         xK_F1), spawn "sudo /home/rleppink/.local/bin/tpb -d")
  , ((modm,         xK_F2), spawn "sudo /home/rleppink/.local/bin/tpb -i")

    -- XF86AudioMute
  , ((0, 0x1008ff12), spawn "amixer set Master toggle > /dev/null")
  , ((modm, xK_F10), spawn "amixer set Master toggle > /dev/null")

    -- XF86AudioRaiseVolume
  , ((0, 0x1008ff13), spawn "amixer set Master 5%+ -M > /dev/null")
  , ((modm, xK_F12), spawn "amixer set Master 5%+ -M > /dev/null")

    -- XF86AudioLowerVolume
  , ((0, 0x1008ff11), spawn "amixer set Master 5%- -M > /dev/null")
  , ((modm, xK_F11), spawn "amixer set Master 5%- -M > /dev/null")
  ]
