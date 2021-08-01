module ParseTree(

) where

import Lexer
import Token.Bracket
import Token.Control
import Token.Data
import Token.Identifier
import Token.Keyword
import Token.Operator

import Data.List

data ParseTree a = Empty | Node a (ParseTree a) (ParseTree a) deriving (Show, Read, Eq)

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

zipTokenScope :: [Token] -> [(Token, Int)]
zipTokenScope ts = zip (ts) (scanTokenScope ts)

filterBracketFromZippedScope :: [(Token, Int)] -> [(Token, Int)]
filterBracketFromZippedScope tss = filter (\x-> not ((fst x) `like` Bracket (Send Open))) tss

insertScopedToken :: (Token,Int) -> ParseTree (Token,Int) -> ParseTree (Token,Int)
insertScopedToken ti Empty = node ti
insertScopedToken ti (Node base left right)
    | (snd ti) > (snd base) = insertScopedToken ti left
    | otherwise             = insertScopedToken ti right

foldToScopedTree :: [(Token, Int)] -> ParseTree (Token,Int)
foldToScopedTree tis = foldr (insertScopedToken) Empty tis

generateScopedTreeFromTokens :: [Token] -> ParseTree (Token,Int)
generateScopedTreeFromTokens ts = foldToScopedTree $ filterBracketFromZippedScope $ zipTokenScope ts