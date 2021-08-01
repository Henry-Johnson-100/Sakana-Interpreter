module Token.Bracket (
Bracket(..),
Scope(..),
validBracket,
fromBracket,
readBracket,
repr,
scope
) where

data Bracket = Send Scope | Return Scope deriving (Show, Read, Eq, Ord)

repr :: [String]
repr = [">(",")>","<(",")<"]

readBracket :: String -> Bracket
readBracket ">(" = Send Open
readBracket ")>" = Send Close
readBracket "<(" = Return Open
readBracket ")<" = Return Close

fromBracket :: Bracket -> String
fromBracket (Send Open)    = ">("
fromBracket (Send Close)   = ")>"
fromBracket (Return Open)  = "<("
fromBracket (Return Close) = ")<"

validBracket :: [Bracket] -> Bool
validBracket brs
    | null brs                        = True
    | length brs /= length singlePass = validBracket singlePass
    | otherwise                       = False
    where
        singlePass = removeAdjacent brs

--------------------------------------------------------------------------------------------------------

data Scope   = Open       | Close        deriving (Show, Read, Eq, Ord)

scope :: Bracket -> Scope
scope (Send s)   = s
scope (Return s) = s

oppBracket :: Bracket -> Bracket
oppBracket (Send Open)    = Send Close
oppBracket (Send Close)   = Send Open
oppBracket (Return Open)  = Return Close
oppBracket (Return Close) = Return Open

isAdjacent :: Bracket -> Bracket -> Bool
isAdjacent b r = b == (oppBracket r) && (scope b == Open)

removeAdjacent :: [Bracket] -> [Bracket]
removeAdjacent [] = []
removeAdjacent (br:brs)
    | null brs = br : []
    | isAdjacent br (head brs) = removeAdjacent $ tail brs
    | otherwise                = br : removeAdjacent brs


