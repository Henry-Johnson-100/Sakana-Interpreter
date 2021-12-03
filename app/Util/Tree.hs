module Util.Tree
  ( Tree (..),
    TreeIO (..),
    tree,
    reTree,
    trees,
    serialTree,
    treeNode,
    treeChildren,
    childrenOfChildren,
    transplantChildren,
    mutateTreeNode,
    childMap,
    (-<-),
    (-<=),
    lookupOn,
    treeMap,
    maybeOnTreeNode,
    nodeStrictlySatisfies,
    firstChild,
    flattenTree,
  )
where

import qualified Data.Maybe (fromJust)
import qualified Util.General

class TreeIO r where
  fPrintTree :: (Show a) => Int -> r a -> String
  ioPrintTree :: (Show a) => r a -> IO ()

data Tree a = Empty | a :-<-: [Tree a] deriving (Show, Eq)

instance TreeIO Tree where
  fPrintTree d Empty =
    concat (replicate (d * 4 - 1) "-")
      ++ ">"
      ++ "Empty\n"
  fPrintTree d (n :-<-: a) =
    concat (replicate (d * 4 - 1) "-")
      ++ ">"
      ++ show n
      ++ "\n"
      ++ concatMap (fPrintTree (d + 1)) a
  ioPrintTree t = putStrLn $ fPrintTree 0 t

instance Functor Tree where
  fmap f (b :-<-: cs) = f b :-<-: map (fmap f) cs

tree :: a -> Tree a
tree x = x :-<-: []

reTree :: Tree a -> Tree a
reTree Empty = Empty
reTree tr = (tree . Data.Maybe.fromJust . treeNode) tr

trees :: [a] -> [Tree a]
trees = map tree

serialTree :: [a] -> Tree a
serialTree [] = Empty
serialTree [x] = tree x
serialTree (x : xs) = tree x -<- serialTree xs

treeNode :: Tree a -> Maybe a
treeNode Empty = Nothing
treeNode (n :-<-: _) = Just n

treeChildren :: Tree a -> [Tree a]
treeChildren Empty = []
treeChildren (_ :-<-: cs) = cs

childrenOfChildren :: Tree a -> [[Tree a]]
childrenOfChildren Empty = []
childrenOfChildren (_ :-<-: cs) = cs : concatMap childrenOfChildren cs

transplantChildren :: Tree a -> Tree a -> Tree a
transplantChildren t (_ :-<-: cs) = t -<= cs

-- | Apply function 'f :: a -> a' to the node of a tree,
-- not affecting the children in any way.
mutateTreeNode :: Tree a -> (a -> a) -> Tree a
mutateTreeNode Empty _ = Empty
mutateTreeNode (n :-<-: cs) f = f n :-<-: cs

childMap :: (Tree a -> b) -> Tree a -> [b]
childMap f tr = map f (treeChildren tr)

(-<-) :: Tree a -> Tree a -> Tree a
Empty -<- t = t
tr -<- t = tr -<= [t]

(-<=) :: Tree a -> [Tree a] -> Tree a
(n :-<-: cs) -<= ts = n :-<-: (cs ++ ts)

lookupOn :: Tree a -> (Tree a -> Bool) -> [Tree a]
lookupOn Empty _ = []
lookupOn t tf
  | tf t = t : concatMap (`lookupOn` tf) (treeChildren t)
  | otherwise = concatMap (`lookupOn` tf) (treeChildren t)

treeMap :: (Tree a -> Tree b) -> Tree a -> Tree b
-- treeMap _ Empty = Empty
-- treeMap f (n :-<-: []) = (f . tree) n
treeMap f tra = (f . reTree) tra -<= map (treeMap f) (treeChildren tra)

maybeOnTreeNode :: b -> (a -> b) -> Tree a -> b
maybeOnTreeNode defaultVal f st = maybe defaultVal f (treeNode st)

nodeStrictlySatisfies :: (a -> Bool) -> Tree a -> Bool
nodeStrictlySatisfies = maybeOnTreeNode False

firstChild :: Tree a -> Maybe (Tree a)
firstChild = Util.General.head' . treeChildren

flattenTree :: Tree a -> [a]
flattenTree Empty = []
flattenTree (n :-<-: []) = [n]
flattenTree (n :-<-: cs) = n : (concatMap flattenTree cs)