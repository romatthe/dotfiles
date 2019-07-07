module Config.Keys where

import Data.Map as M

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Config.Prime
import XMonad.Core
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import Graphics.X11.Types

import Config.Options

keybindings =
  --[ ((m, xK_b), spawn "google-chrome-stable")
  [ ("M-b",        spawn "google-chrome-stable")
  , ("M-<Space>",  spawn "rofi -show drun -theme romatthe")
  , ("M-<Return>", spawn $ term options)
  , ("M-w",        kill)
  , ("M-f",        sendMessage $ Toggle FULL)
  , ("M-<Right>",  windowGo R False)
  , ("M-<Left>",   windowGo L False)
  , ("M-<Up>",     windowGo U False)
  , ("M-<Down>",   windowGo D False)
  , ("M-s",        sendMessage ToggleStruts)
  , ("M-S-t",      sendMessage NextLayout)
  ]
