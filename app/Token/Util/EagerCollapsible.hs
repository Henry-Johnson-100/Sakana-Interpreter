module Token.Util.EagerCollapsible
  ( takeBetween,
    dropBetween,
    isEagerCollapsible,
    dropInfix,
  )
where

import qualified Data.List (isPrefixOf)
import qualified Token.Util.CollapsibleTerminalCases as CTC
  ( CollapsibleTerminalCases (..),
    sameCase,
  )

isEagerCollapsible :: CTC.CollapsibleTerminalCases a -> [a] -> Bool
isEagerCollapsible _ [] = False
isEagerCollapsible ctc xs = foldEC ctc False xs

foldEC :: CTC.CollapsibleTerminalCases a -> Bool -> [a] -> Bool
foldEC _ _ [] = False
foldEC ctc bool (x : xs)
  | not bool && (CTC.endCase ctc) x && not ((CTC.sameCase ctc) x) = False
  | not bool && ((CTC.beginCase ctc) x || (CTC.sameCase ctc) x) = foldEC ctc True xs
  | bool && (CTC.beginCase ctc) x && not ((CTC.sameCase ctc) x) = False
  | bool && ((CTC.endCase ctc) x || (CTC.sameCase ctc) x) = True
  | otherwise = foldEC ctc bool xs

-- | For elements in a list where f(x) is True,
-- take that element and all elements after it until the next such element.
-- Works inclusively, the list returned has the property f(head xs) == True
-- and f(end xs) == true and every element in between evaluates to False.
takeBetween :: (Eq a) => CTC.CollapsibleTerminalCases a -> [a] -> [a]
takeBetween _ [] = []
takeBetween ctc xs
  | not (isEagerCollapsible ctc xs) = xs
  | notBeginCase (head xs) = takeBetween ctc (dropWhile notBeginCase xs)
  | otherwise =
    head xs : takeWhile notEndCase (tail xs) ++ [head (dropWhile notEndCase (tail xs))]
  where
    notBeginCase = (not . CTC.beginCase ctc)
    notEndCase = (not . CTC.endCase ctc)

-- | take two lists, the first being a list infixed in the second,
-- return a list with that infix removed
-- Drops nothing if the first list is not an infix in the second list
dropInfix :: (Eq a) => [a] -> [a] -> [a]
dropInfix _ [] = []
dropInfix [] xs = xs
dropInfix infixList (x : xs)
  | not (infixList `Data.List.isPrefixOf` (x : xs)) = x : dropInfix infixList xs
  | otherwise = dropInfix (tail infixList) xs

-- | For elements in a list where f(x) is True, drop that element
-- and all elements after it until the next such element.
-- Works inclusively, the list returned has the property f(head xs) == True
-- and f(end xs) == true and every element in between evaluates to False.
dropBetween :: (Eq a) => CTC.CollapsibleTerminalCases a -> [a] -> [a]
dropBetween ctc xs = dropInfix (takeBetween ctc xs) xs
