module Token.Util.NestedCollapsible (
    isCompleteNestedCollapsible,
    hasNestedCollapsible,
    takeNest
) where

import Data.List

isCompleteNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isCompleteNestedCollapsible beginCase endCase xs = (fst terminations) == (snd terminations) && (fst terminations) /= 0 && (beginCase (head xs)) && (endCase (last xs)) where 
    terminations = numberOfTerminations beginCase endCase 0 0 xs

hasNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
hasNestedCollapsible beginCase endCase xs = all (0 < ) [fst terminations, snd terminations] where
    terminations = numberOfTerminations beginCase endCase 0 0 xs

takeNest :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeNest _ _ [] = []
takeNest beginCase endCase (x:xs)
    | not (hNC (x:xs))                        = []
    | iCNC (x:xs)                             = takeNest beginCase endCase xs
    | (fst terminations) > (snd terminations) = takeNest beginCase endCase xs
    | otherwise                               = takeUntilTerminationLevel endCase (minTuple terminations) (dropWhile (\xx -> not (beginCase xx)) (x:xs))
    where
        hNC ncs = hasNestedCollapsible beginCase endCase ncs
        iCNC ncs = isCompleteNestedCollapsible beginCase endCase ncs
        terminations = numberOfTerminations beginCase endCase 0 0 (x:xs)

takeUntilTerminationLevel :: (a -> Bool) -> Int -> [a] -> [a]
takeUntilTerminationLevel _ 0 _ = []
takeUntilTerminationLevel _ _ [] = []
takeUntilTerminationLevel termCase termLevel (x:xs)
    | termCase x = x : takeUntilTerminationLevel termCase (termLevel - 1) xs
    | otherwise  = x : takeUntilTerminationLevel termCase termLevel xs

minTuple :: (Int, Int) -> Int
minTuple (a, b) = minimum [a,b]

numberOfTerminations :: (a -> Bool) -> (a -> Bool) -> Int -> Int -> [a] -> (Int,Int)
numberOfTerminations _ _ begins ends [] = (begins, ends)
numberOfTerminations beginCase endCase begins ends (x:xs)
    | beginCase x = numberOfTerminations beginCase endCase (begins + 1) ends       xs
    | endCase   x = numberOfTerminations beginCase endCase begins       (ends + 1) xs
    | otherwise   = numberOfTerminations beginCase endCase begins       ends       xs