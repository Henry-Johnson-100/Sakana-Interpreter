module Token.Util.Tree where

class TreeIO r where
  fPrintTree :: (Show a) => Int -> r a -> String
  ioPrintTree :: (Show a) => r a -> IO ()

data Tree a = Empty | a :-<-: [Tree a] | Headless [Tree a] deriving (Show, Eq)

instance TreeIO Tree where
  fPrintTree d Empty = concat (replicate (d * 4 - 1) "-") ++ ">" ++ "Empty\n"
  fPrintTree d (Headless a) = concat (replicate (d * 4 - 1) "-") ++ ">" ++ "HEADLESS\n" ++ concatMap (fPrintTree (d + 1)) a
  fPrintTree d (n :-<-: a) = concat (replicate (d * 4 - 1) "-") ++ ">" ++ show n ++ "\n" ++ concatMap (fPrintTree (d + 1)) a
  ioPrintTree t = putStrLn $ fPrintTree 0 t

instance Functor Tree where
  fmap f (b :-<-: cs) = f b :-<-: map (fmap f) cs

isHeadless :: Tree a -> Bool
isHeadless (Headless _) = True
isHeadless _ = False

tree :: a -> Tree a
tree x = x :-<-: []

trees :: [a] -> [Tree a]
trees = map (:-<-: [])

serialTree :: [a] -> Tree a
serialTree [] = Empty
serialTree [x] = tree x
serialTree (x : xs) = tree x -<- serialTree xs

treeNode :: Tree a -> a
treeNode (b :-<-: _) = b

treeChildren :: Tree a -> [Tree a]
treeChildren (_ :-<-: cs) = cs
treeChildren (Headless cs) = cs

transplantChildren :: Tree a -> Tree a -> Tree a
transplantChildren t (_ :-<-: cs) = t -<= cs

(-<-) :: Tree a -> Tree a -> Tree a
t -<- (Headless cs) = t -<= cs
(b :-<-: cs) -<- t = b :-<-: (cs ++ [t])
(Headless cs) -<- t = Headless (cs ++ [t])

(-<=) :: Tree a -> [Tree a] -> Tree a
(b :-<-: cs) -<= ts = b :-<-: (cs ++ ts)
(Headless cs) -<= ts = Headless (cs ++ ts)

lookupOn :: Tree a -> (Tree a -> Bool) -> [Tree a]
lookupOn Empty _ = [Empty]
lookupOn t tf
    | tf t = [t]
    | otherwise = concatMap (`lookupOn` tf) (treeChildren t)