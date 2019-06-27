import Data.List
import System.IO

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Circle
import XMonad.Layout.Gaps
import qualified XMonad.StackSet as W
import XMonad.Util.CustomKeys
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce


main = do
	bar <- spawnPipe panel
	-- info <- spawnPipe myRightBar
	xmonad $ defaultConfig
		{ manageHook = manageDocks <+> myManageHook
		, layoutHook = windowArrange layout
		-- , startupHook = startUp
		, workspaces = myWorkspaces
		, modMask = mod4Mask
		, terminal = "gnome-terminal"
		, borderWidth = 7
		, focusedBorderColor = "#6A555C" --"#404752"
		, normalBorderColor = "#404752" --"#343C48"
		, logHook = logbar bar
    , keys = myKeys
		}

-- Workspaces plus clickable functionality
myWorkspaces 	:: [String]
myWorkspaces	= click $ [ " PHOS ", " ETHE ", " ASTR ", " THEE ", " CRES "]
		  where click l = [ "^ca(1, xdotool key super+"
				  ++ show (n) ++ ")" ++ ws ++ "^ca()" |
				  (i,ws) <- zip [1..] l,
				  let n = i]

myManageHook = composeAll
	[ className =? "feh" --> doFloat
	, className =? "mpv"	--> doFloat
	, manageDocks
	]

-- startUp :: X()
-- startUp = do
--	spawnOnce "compton"
--	spawnOnce "./.fehbg"
--	spawnOnce "xsetroot -cursor_name left_ptr"
--	spawnOnce "xrdb -load .Xresources"
--	spawnOnce "mpd"
--	setWMName "LG3D"

logbar h = dynamicLogWithPP $ tryPP h

tryPP :: Handle -> PP
tryPP h = defaultPP
	{ ppOutput		= hPutStrLn h
	, ppCurrent		= dzenColor (fore) (blu1) . pad
	, ppVisible		= dzenColor (fore) (back) . pad
	, ppHidden		= dzenColor (fore) (back) . pad
	, ppHiddenNoWindows	= dzenColor (fore) (back) . pad
	, ppUrgent		= dzenColor (fore) (red1) . pad
	, ppOrder		= \(ws:l:t) -> [ws,l]
	, ppSep			= ""
	, ppWsSep       = ""
	, ppLayout		= dzenColor (fore) (red1) .
				( \t -> case t of
					"Spacing 2 ResizableTall" -> " " ++ i ++ "tile.xbm) TALL "
					"Full" -> " " ++ i ++ "dice1.xbm) FULL "
					"Circle" -> " " ++ i ++ "dice2.xbm) CIRC "
					_ -> " " ++ i ++ "tile.xbm) TALL "
				)
	} where i = "^i(~/.xmonad/icons/stlarch/"

-- Colors
blu1 = "#528588"
red1 = "#BA5E57"
fore = "#DEE3E0"
back = "#343C48"

-- Layour
res = ResizableTall 1 (2/100) (1/2) []
ful = noBorders (fullscreenFull Full)

   -- useless gap --
space = spacingRaw True (Border 50 20 20 20) True (Border 15 15 15 15) True
layout = space $ avoidStruts (spacing 2 $ res) ||| Circle ||| ful

panel = "dzen2 -ta l -p -w 337 -x 30 -y 15 -h 28 -e "
-- myRightBar = "conky -c ~/.conkyrc | dzen2 -x 367 -y 15 -h 28 -w 1528 -p -ta r -e "

-- Keybindings
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
