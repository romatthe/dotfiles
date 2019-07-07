module Hooks.Log where

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (join)

import XMonad
import qualified XMonad.StackSet as W

import Theme.Nord


wsLogHook :: X ()
wsLogHook = do
  winset <- gets windowset
  -- workspaces
  let currWs = W.currentTag winset
  -- blocking named scratchpad appearing
  let wss = filter (/= "NSP") $ map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss
  -- fifo
  io $ appendFile "/tmp/xmonad-ws" (wsStr ++ "\n")
  where
    fmt currWs ws
      -- %{T3} changes font to bold in polybar
      -- %{T-} resets it back to font-0
      -- NOTE: Foreground colours also edited here
      -- this block then depends on +THEME+
      | currWs == ws = " %{F"  ++ base00 ++ "}%{T3}[" ++ ws ++ "]%{T-}%{F-} "
      | otherwise    = "  %{F" ++ base10 ++ "}%{T4}"  ++ ws ++ "%{T-}%{F-}  "

    sort' = sortBy (compare `on` (!! 0))
