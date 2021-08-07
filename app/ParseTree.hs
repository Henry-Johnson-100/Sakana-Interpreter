module ParseTree(
ParseTree(..),
generateParseTree
) where

import Lexer
import Token.Bracket
import Token.Control
import Token.Data
import Token.Keyword
import Token.Operator

import Data.List

data ParseTree a = Empty | Node a (ParseTree a) (ParseTree a) deriving (Show, Read, Eq)

type ScopedToken = (Token, Int)
type ScopedTree = ParseTree ScopedToken

node :: a -> ParseTree a
node x = Node x Empty Empty

instance Functor ParseTree where
    fmap _ Empty                  = Empty
    fmap f (Node base left right) = Node (f base) (fmap f left) (fmap f right)


scanTokenScope :: [Token] -> [Int]
scanTokenScope ts = tail $ scanl (scanf) 0 ts
    where
        scanf :: Int -> Token -> Int
        scanf i t
            | t `like` (Bracket (Send Open)) && ((scope (baseBracket t)) == Open)  = i + 1
            | t `like` (Bracket (Send Open)) && ((scope (baseBracket t)) == Close) = i - 1
            | otherwise                                                            = i

zipTokenScope :: [Token] -> [ScopedToken]
zipTokenScope ts = zip (ts) (scanTokenScope ts)

filterNotBracket :: [ScopedToken] -> [ScopedToken]
filterNotBracket tss = filter (\x-> not ((fst x) `like` Bracket (Send Open))) tss

insertScopedToken :: ScopedToken -> ScopedTree -> ScopedTree
insertScopedToken ti Empty = node ti
insertScopedToken ti (Node base left right)
    | (snd ti) > (snd base) = Node base (insertScopedToken ti left) right
    | otherwise             = Node base left (insertScopedToken ti right)

foldToScopedTree :: [ScopedToken] -> ScopedTree
foldToScopedTree tis = foldl' (flip (insertScopedToken)) Empty tis

generateScopedTreeFromTokens :: [Token] -> ScopedTree
generateScopedTreeFromTokens ts = foldToScopedTree $ filterNotBracket $ zipTokenScope ts

generateParseTree :: [Token] -> ParseTree Token
generateParseTree ts = Empty