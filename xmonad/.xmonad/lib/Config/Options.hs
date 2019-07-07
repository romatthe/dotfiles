module Config.Options where

import Graphics.X11.Types

data Options = Options
  { term   :: String
  , ffm    :: Bool
  , mask   :: KeyMask
  , spaces :: [String]
  }

options :: Options
options = Options
  { term   = "termite"
  , ffm    = True
  , mask   = mod4Mask
  , spaces = spaces'
  }

spaces' :: [[Char]]
spaces' = ws $ [ "TERM", "INET", "DEV", "ENT", "PLAY", "TOOL"]
  where
    ws l =
      [ "^ca(1,xdotool key super+" ++ show n ++ ")  " ++ wsp ++ "  ^ca()"
      | (i, wsp) <- zip [1 ..] l
      , let n = i
      ]
