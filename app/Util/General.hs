module Util.General
  ( both,
    head',
    tail',
    init',
    last',
    foldIdApplicativeOnSingleton,
    listSingleton,
    rotateArg3,
    (.<),
  )
where

import Data.Maybe (Maybe)

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

head' :: [a] -> Maybe a
head' xs = if null xs then Nothing else (Just . head) xs

tail' :: [a] -> [a]
tail' xs = if null xs then [] else tail xs

init' :: [a] -> [a]
init' xs = if null xs then [] else init xs

last' :: [a] -> Maybe a
last' xs = if null xs then Nothing else (Just . last) xs

foldIdApplicativeOnSingleton :: ((a1 -> a1) -> [b] -> c) -> [a2 -> b] -> a2 -> c
foldIdApplicativeOnSingleton foldF funcAtoB = foldF id . (funcAtoB <*>) . listSingleton

listSingleton :: a -> [a]
listSingleton = flip (:) []

rotateArg3 :: (t1 -> t2 -> t3 -> t4) -> t2 -> t3 -> t1 -> t4
rotateArg3 f b c a = f a b c

-- | Works like function composition but composes a function taking one argument, (g),
-- to a function, (f), which takes two arguments.
--
-- So that,
--
-- > g .< f
--
-- Is equivalent to:
--
-- > (g .) . f
--
-- Or
--
-- > g (f x y)
(.<) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.<) g fxy = (g .) . fxy