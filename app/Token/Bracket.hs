module Token.Bracket (
BracketTerminal(..),
ScopeType(..),
fromBracket,
readBracket,
repr
) where

data Bracket = Bracket {
    scopeType :: ScopeType,
    bracketTerminal :: BracketTerminal
} deriving (Show, Read, Eq, Ord)


data BracketTerminal   = Open       | Close        deriving (Show, Read, Eq, Ord)


data ScopeType = Send | Return deriving (Show, Read, Eq, Ord)


repr :: [String]
repr = [">(",")>","<(",")<"]


readBracket :: String -> (ScopeType, BracketTerminal)
readBracket ">(" = (Send, Open)
readBracket ")>" = (Send, Close)
readBracket "<(" = (Return, Open)
readBracket ")<" = (Return, Close)


fromBracket :: ScopeType -> BracketTerminal -> String
fromBracket Send Open    = ">("
fromBracket Send Close   = ")>"
fromBracket Return Open  = "<("
fromBracket Return Close = ")<"


-- validBracket :: [Bracket] -> Bool
-- validBracket brs
--     | null brs                        = True
--     | length brs /= length singlePass = validBracket singlePass
--     | otherwise                       = False
--     where
--         singlePass = removeAdjacent brs

--------------------------------------------------------------------------------------------------------


-- oppBracket :: Bracket -> Bracket
-- oppBracket (Send Open)    = Send Close
-- oppBracket (Send Close)   = Send Open
-- oppBracket (Return Open)  = Return Close
-- oppBracket (Return Close) = Return Open


-- isAdjacent :: Bracket -> Bracket -> Bool
-- isAdjacent b r = b == (oppBracket r) && (bracketTerminal b == Open)


-- removeAdjacent :: [Bracket] -> [Bracket]
-- removeAdjacent [] = []
-- removeAdjacent (br:brs)
--     | null brs = br : []
--     | isAdjacent br (head brs) = removeAdjacent $ tail brs
--     | otherwise                = br : removeAdjacent brs


