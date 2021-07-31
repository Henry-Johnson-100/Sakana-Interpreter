module AbstractSyntaxTree(

) where

import Lexer

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

testTree = (Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 4 (Node 5 Empty Empty) Empty))

instance Functor Tree where
    fmap _ Empty                   = Empty
    fmap f (Node a (left) (right)) = (Node (f a) (fmap f left) (fmap f right))

rightBranch :: Tree a -> Tree a
rightBranch (Node _ _ right) = right
rightBranch Empty = Empty


leftBranch :: Tree a -> Tree a
leftBranch (Node _ left _) = left
leftBranch Empty = Empty


isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False


node :: a -> Tree a
node x = Node x Empty Empty


insTreeRight :: Tree a -> Tree a -> Tree a
insTreeRight x (Node base left right) = Node base left x
insTreeRight x Empty                  = x


insTreeLeft :: Tree a -> Tree a -> Tree a
insTreeLeft x (Node base left right) = Node base x right
insTreeLeft x Empty                  = x


insRight :: a -> Tree a -> Tree a
insRight x tree = insTreeRight (node x) tree


insLeft :: a -> Tree a -> Tree a
insLeft x tree = insTreeLeft (node x) tree

