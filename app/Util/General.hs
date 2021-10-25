module Util.General
  ( both,
    head',
    tail',
    init',
    last',
    foldIdApplicativeOnSingleton,
    listSingleton,
    rotateArg3,
  )
where

import Data.Maybe

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = (Just . head) xs

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = (Just . last) xs

foldIdApplicativeOnSingleton :: ((a1 -> a1) -> [b] -> c) -> [a2 -> b] -> a2 -> c
foldIdApplicativeOnSingleton foldF funcAtoB = foldF id . (funcAtoB <*>) . listSingleton

listSingleton :: a -> [a]
listSingleton x = [x]

rotateArg3 :: (t1 -> t2 -> t3 -> t4) -> t2 -> t3 -> t1 -> t4
rotateArg3 f b c a = f a b c