module Token.Bracket
  ( BracketTerminal (..),
    ScopeType (..),
    fromBracket,
    readBracket,
    readScopeType,
    repr,
  )
where

data Bracket = Bracket
  { scopeType :: ScopeType,
    bracketTerminal :: BracketTerminal
  }
  deriving (Show, Read, Eq, Ord)

data BracketTerminal = Open | Close deriving (Show, Read, Eq, Ord)

data ScopeType = Send | Return deriving (Show, Read, Eq, Ord)

repr :: [String]
repr = [">(", ")>", "<(", ")<"]

readScopeType :: Char -> ScopeType
readScopeType '>' = Send
readScopeType '<' = Return

readBracket :: String -> (ScopeType, BracketTerminal)
readBracket ">(" = (Send, Open)
readBracket ")>" = (Send, Close)
readBracket "<(" = (Return, Open)
readBracket ")<" = (Return, Close)

fromBracket :: ScopeType -> BracketTerminal -> String
fromBracket Send Open = ">("
fromBracket Send Close = ")>"
fromBracket Return Open = "<("
fromBracket Return Close = ")<"