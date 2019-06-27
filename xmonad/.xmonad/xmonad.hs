import XMonad


import Data.List
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Config
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.FixedColumn
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.CustomKeys
import XMonad.Util.Run


main :: IO ()
main = do
  xmobarPipe <- spawnPipe xmobarCommand
  xmonad
    $ withNavigation2DConfig def { layoutNavigation = [("BSP", hybridNavigation)] }
    $ myConfig { logHook = dynamicLogWithPP $ myXmobarPP xmobarPipe }


-- TODO: Get these colors from xrdb
backgroundColor   = "#FEFEFE"
middleColor       = "#AEAEAE"
foregroundColor   = "#0E0E0E"

myConfig = ewmh def
  { borderWidth        = 5
  , focusedBorderColor = "#FFFFFF"
  , focusFollowsMouse  = False
  , handleEventHook    = docksEventHook <+> fullscreenEventHook
  , keys               = myKeys
  , layoutHook         = avoidStruts $ smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ spacingWithEdge 4 emptyBSP
  , manageHook         = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
  , modMask            = mod4Mask
  , normalBorderColor  = middleColor
  , terminal           = "gnome-terminal"
  , workspaces         = [ "browse", "code", "read", "chat", "etc" ]
  }

myXmobarPP xmobarPipe = defaultPP
  { ppCurrent         = pad . xmobarColor foregroundColor  ""
  , ppHidden          = pad . xmobarColor middleColor ""
  , ppHiddenNoWindows = pad . xmobarColor middleColor ""
  , ppLayout          = const ""
  , ppOutput          = hPutStrLn xmobarPipe
  , ppTitle           = const ""
  , ppVisible         = pad . xmobarColor middleColor ""
  , ppWsSep           = " "
  }

xmobarCommand :: String
xmobarCommand = "xmobar ~/.xmonad/xmobarrc"

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

  -- Fullscreen toggle
  , ((modm, xK_f), sendMessage $ Toggle FULL)

    -- Directional navigation of windows
  , ((modm, xK_l), windowGo R False)
  , ((modm, xK_h), windowGo L False)
  , ((modm, xK_k), windowGo U False)
  , ((modm, xK_j), windowGo D False)
  , ((modm, xK_Right), windowGo R False)
  , ((modm, xK_Left),  windowGo L False)
  , ((modm, xK_Up),    windowGo U False)
  , ((modm, xK_Down),  windowGo D False)

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
