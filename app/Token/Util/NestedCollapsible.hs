module Token.Util.NestedCollapsible (

) where

import Data.List

data Nested a = NBase [a] | Nest (Nested a) deriving (Show,Read,Eq)

retrieveBase :: Nested a -> [a]
retrieveBase (NBase xs)   = xs
retrieveBase (Nest bxs) = retrieveBase bxs

unNestAll :: [Nested a] -> [a]
unNestAll nxs = concat $ map (retrieveBase) nxs

instance Functor Nested where
    fmap f (NBase xs) = nBase $ fmap f xs
    fmap f (Nest nxs) = nest $ fmap f nxs

instance Applicative Nested where
    pure x = NBase (x:[])
    (NBase fxs) <*> (NBase xs) = nBase (fxs <*> xs)
    (Nest nfxs) <*> nbxs       = Nest $ nfxs <*> nbxs
    nfxs        <*> (Nest bxs) = Nest $ nfxs <*> bxs

instance Monad Nested where
    return x = NBase (x:[])
    (NBase xs) >>= f = NBase $ unNestAll $ map f xs
    (Nest bxs) >>= f = Nest $ (bxs >>= f)

nBase :: [a] -> Nested a
nBase xs = NBase xs

nest :: Nested a -> Nested a
nest x = Nest x

isNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isNestedCollapsible _ _ _ = False