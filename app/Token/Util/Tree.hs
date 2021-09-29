module Token.Util.Tree where

import Token.Util.Like

class TreeIO r where
  fPrintTree :: (Show a) => Int -> r a -> String
  ioPrintTree :: (Show a) => r a -> IO ()

data Tree a = Empty | a :-<-: [Tree a] deriving (Show, Eq)

instance TreeIO Tree where
  fPrintTree d Empty = concat (replicate (d * 4 - 1) "-") ++ ">" ++ "Empty\n"
  fPrintTree d (n :-<-: a) = concat (replicate (d * 4 - 1) "-") ++ ">" ++ show n ++ "\n" ++ concatMap (fPrintTree (d + 1)) a
  ioPrintTree t = putStrLn $ fPrintTree 0 t

instance Functor Tree where
  fmap f (b :-<-: cs) = f b :-<-: map (fmap f) cs

tree :: a -> Tree a
tree x = x :-<-: []

reTree :: Tree a -> Tree a
reTree = tree . treeNode

trees :: [a] -> [Tree a]
trees = map (:-<-: [])

serialTree :: [a] -> Tree a
serialTree [] = Empty
serialTree [x] = tree x
serialTree (x : xs) = tree x -<- serialTree xs

treeNode :: Tree a -> a
treeNode (n :-<-: _) = n

treeChildren :: Tree a -> [Tree a]
treeChildren (_ :-<-: cs) = cs

transplantChildren :: Tree a -> Tree a -> Tree a
transplantChildren t (_ :-<-: cs) = t -<= cs

mutateTreeNode :: Tree a -> (a -> a) -> Tree a
mutateTreeNode Empty _ = Empty
mutateTreeNode (n :-<-: cs) f = f n :-<-: cs

childMap :: (Tree a -> b) -> Tree a -> [b]
childMap f tr = map f (treeChildren tr)

(-<-) :: Tree a -> Tree a -> Tree a
(b :-<-: cs) -<- t = b :-<-: (cs ++ [t])

(-<=) :: Tree a -> [Tree a] -> Tree a
(b :-<-: cs) -<= ts = b :-<-: (cs ++ ts)

lookupOn :: Tree a -> (Tree a -> Bool) -> [Tree a]
lookupOn Empty _ = []
lookupOn t tf
  | tf t = t : concatMap (`lookupOn` tf) (treeChildren t)
  | otherwise = concatMap (`lookupOn` tf) (treeChildren t)

treeElem :: (Eq a) => Tree a -> a -> Bool
treeElem tr check = (not . null) (lookupOn tr (\x -> treeNode x == check))