module Token.Util.EagerCollapsible(
takeBetween,
dropBetween,
isEagerCollapsible,
dropInfix
) where

import Data.List (delete, isInfixOf, foldl',isPrefixOf)

-- |
isEagerCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isEagerCollapsible _ _ []                            = False
isEagerCollapsible beginCase endCase xs
    | not (any beginCase xs) || not (any endCase xs) = False
    | takeWhileBeginCaseL < takeWhileEndCaseL        = True
    | takeWhileBeginCaseL == takeWhileEndCaseL       = isEagerCollapsible beginCase endCase (dropped)
    | otherwise                                      = False
    where 
        takeWhileBeginCaseL = length $ takeWhile (\x -> not (beginCase x)) xs
        takeWhileEndCaseL   = length $ takeWhile (\x -> not (endCase x))   xs
        dropped             = tail $ dropWhile (\x -> not (endCase x)) xs


-- | For elements in a list where f(x) is True, take that element and all elements after it until the next such element.
-- | Works inclusively, the list returned has the property f(head xs) == True and f(end xs) == true and every element in between evaluates to False.
-- So the issue I think is that takeBetween works for lists with multiple instances of beginCase and endCase which is good
-- but not in the case of strings since a single string Data type will always count as an eagerCollapsible
-- So I need to change my isStringPrefix* functions or make it so that takeBetween only works for one instance.
takeBetween :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a] {---#TODO Implement isEagerCollapsible in these function guards-}
takeBetween _ _ [] = []
takeBetween beginCase afterCase (x:xs)
    | beginCase x && isEagerCollapsible beginCase afterCase xs = between ++ takeBetween beginCase afterCase (after)
    | otherwise                                                = takeBetween beginCase afterCase xs
    where
        breakList = break (afterCase) xs
        between   = x : (fst breakList) ++ ((head (snd breakList)) : [])
        after     = tail (snd breakList)


-- | take two lists, the first being a list infixed in the second, return a list with that infix removed
-- | Drops nothing if the first list is not an infix in the second list
dropInfix :: (Eq a) => [a] -> [a] -> [a]
dropInfix _  [] = []
dropInfix [] xs = xs
dropInfix infixList (x:xs)
    | not (isPrefixOf infixList (x:xs)) = x : dropInfix infixList xs
    | otherwise                         = dropInfix (tail infixList) (xs)


-- | For elements in a list where f(x) is True, drop that element and all elements after it until the next such element.
-- | Works inclusively, the list returned has the property f(head xs) == True and f(end xs) == true and every element in between evaluates to False.
dropBetween :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a]
dropBetween beginCase endCase xs = dropInfix (takeBetween beginCase endCase xs) xs
