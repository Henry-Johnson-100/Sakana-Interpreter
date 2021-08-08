module Token.Util.NestedCollapsible (
    Nested(..)
) where

import Data.List

data Nested a = Inner [a] | Nest [Nested a] deriving (Show,Read,Eq)

toInner :: [a] -> Nested a
toInner xs = Inner xs

nestSingleton :: Nested a -> Nested a
nestSingleton nxs = Nest (nxs : [])

instance Functor Nested where
    fmap f (Inner xs) = Inner $ fmap f        xs
    fmap f (Nest nxs) = Nest  $ fmap (fmap f) nxs

{-
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
-}