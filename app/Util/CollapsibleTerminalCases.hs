module Util.CollapsibleTerminalCases
  ( CollapsibleTerminalCases (..),
    sameCase,
  )
where

data CollapsibleTerminalCases a = CollapsibleTerminalCases
  { beginCase :: a -> Bool,
    endCase :: a -> Bool
  }

sameCase :: CollapsibleTerminalCases a -> a -> Bool
sameCase ctc x = all id $ [beginCase, endCase] <*> [ctc] <*> [x]