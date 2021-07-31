module AbstractSyntaxTree(

) where

import Lexer

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

testTree = (Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 4 (Node 5 Empty Empty) Empty))

instance Functor Tree where
    fmap _ Empty                   = Empty
    fmap f (Node a (left) (right)) = (Node (f a) (fmap f left) (fmap f right))