module AbstractSyntaxTree(

) where

import Lexer

data Tree a = Empty | Leaf a (Tree a) (Tree a) deriving (Show,Read,Eq)

testTree = (Leaf 1 (Leaf 2 (Leaf 3 Empty Empty) Empty) (Leaf 4 (Leaf 5 Empty Empty) Empty))

instance Functor Tree where
    fmap _ Empty                   = Empty
    fmap f (Leaf a (left) (right)) = (Leaf (f a) (fmap f left) (fmap f right))