module Token.Util.EagerCollapsible
  ( takeBetween,
    dropBetween,
    isEagerCollapsible,
    dropInfix,
  )
where

import Data.List (delete, foldl', isInfixOf, isPrefixOf)

isEagerCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isEagerCollapsible _ _ [] = False
isEagerCollapsible beginCase endCase xs = foldEC beginCase endCase False xs

foldEC :: (a -> Bool) -> (a -> Bool) -> Bool -> [a] -> Bool
foldEC _ _ _ [] = False
foldEC beginCase endCase bool (x : xs)
  | not bool && endCase x && not (sameCase x) = False
  | not bool && (beginCase x || sameCase x) = foldEC beginCase endCase True xs
  | bool && beginCase x && not (sameCase x) = False
  | bool && (endCase x || sameCase x) = True
  | otherwise = foldEC beginCase endCase bool xs
  where
    sameCase t = beginCase t && endCase t

-- | For elements in a list where f(x) is True, take that element and all elements after it until the next such element.
-- | Works inclusively, the list returned has the property f(head xs) == True and f(end xs) == true and every element in between evaluates to False.
takeBetween :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeBetween _ _ [] = []
takeBetween beginCase endCase xs
  | not (isEagerCollapsible beginCase endCase xs) = xs
  | notBeginCase (head xs) = takeBetween beginCase endCase (dropWhile notBeginCase xs)
  | otherwise = head xs : takeWhile notEndCase (tail xs) ++ [head (dropWhile notEndCase (tail xs))]
  where
    notBeginCase x = not $ beginCase x
    notEndCase x = not $ endCase x

-- | take two lists, the first being a list infixed in the second, return a list with that infix removed
-- | Drops nothing if the first list is not an infix in the second list
dropInfix :: (Eq a) => [a] -> [a] -> [a]
dropInfix _ [] = []
dropInfix [] xs = xs
dropInfix infixList (x : xs)
  | not (infixList `isPrefixOf` (x : xs)) = x : dropInfix infixList xs
  | otherwise = dropInfix (tail infixList) xs

-- | For elements in a list where f(x) is True, drop that element and all elements after it until the next such element.
-- | Works inclusively, the list returned has the property f(head xs) == True and f(end xs) == true and every element in between evaluates to False.
dropBetween :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a]
dropBetween beginCase endCase xs = dropInfix (takeBetween beginCase endCase xs) xs
