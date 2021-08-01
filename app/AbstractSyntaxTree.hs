module AbstractSyntaxTree(

) where

import Lexer
import Token.Bracket
import Token.Data
import Token.Operator
import Token.Keyword
import Token.Control

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

instance Functor Tree where
    fmap _ Empty                   = Empty
    fmap f (Node a (left) (right)) = (Node (f a) (fmap f left) (fmap f right))

node :: a -> Tree a
node x = Node x Empty Empty


scopeDepthScan :: [Token] -> [Int]
scopeDepthScan (ts) = scanl incrScopeDepth 0 ts
    where
        incrScopeDepth :: Int -> Token -> Int
        incrScopeDepth acc t
            | t `like` (Bracket (Send Open)) && (scope (baseBracket t)) == Open = acc + 1
            | t `like` (Bracket (Send Open)) && (scope (baseBracket t)) == Close = acc - 1
            | otherwise = acc


zipTokensToScope :: [Token] -> [(Token, Int)]
zipTokensToScope ts = zip (ts) (tail (scopeDepthScan ts))