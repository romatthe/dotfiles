module Hooks.Log where

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (join)

import XMonad
import qualified XMonad.StackSet as W

import Config.Options
import Theme.Nord


wsLogHook :: X ()
wsLogHook = do
  winset <- gets windowset
  let currWs = W.currentTag winset
  let wss = spaces options
  let wsStr = join $ map (fmt currWs) wss
  io $ appendFile "/tmp/xmonad-ws" (wsStr ++ "\n")
  where
    fmt currWs ws
      | currWs == ws = " %{B"  ++ base00 ++ "}%{T3}[" ++ ws ++ "]%{T-}%{B-} "
      | otherwise    = "  %{F" ++ base10 ++ "}%{T4}"  ++ ws ++ "%{T-}%{F-}  "
