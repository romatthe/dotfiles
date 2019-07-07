module Config.Options where

import XMonad.Core
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce

import Graphics.X11.Types

import Apps.Alias

data Options = Options
  { term   :: String
  , ffm    :: Bool
  , mask   :: KeyMask
  , spaces :: [String]
  , starts :: X ()
  }

options :: Options
options = Options
  { term   = "termite"
  , ffm    = True
  , mask   = mod4Mask
  , spaces = spaces'
  , starts = ewmhDesktopsStartup
           >> setWMName "XMonad"
           >> spawnOnce panel
  }

spaces' :: [[Char]]
spaces' = [ "TERM", "INET", "DEV", "ENT", "PLAY", "TOOL"]
-- spaces' = ws $ [ "TERM", "INET", "DEV", "ENT", "PLAY", "TOOL"]
--   where
--     ws l =
--       [ "^ca(1,xdotool key super+" ++ show n ++ ")  " ++ wsp ++ "  ^ca()"
--       | (i, wsp) <- zip [1 ..] l
--       , let n = i
--      ]
