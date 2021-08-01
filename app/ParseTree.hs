module ParseTree(

) where

import Lexer
import Token.Bracket
import Token.Control
import Token.Data
import Token.Identifier
import Token.Keyword
import Token.Operator

data ParseTree a = Empty | Node a (ParseTree a) (ParseTree a) deriving (Show, Read, Eq)

instance Functor ParseTree where
    fmap f Empty = f Empty
    fmap f (Node base left right) = Node (f base) (fmap f left) (fmap f right)