module Token.Util.NestedCollapsible (
    isCompleteNestedCollapsible,
    hasNestedCollapsible,
    takeNest
) where

import Data.List

isCompleteNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isCompleteNestedCollapsible beginCase endCase xs = (fst terminations) == (snd terminations) && (fst terminations) /= 0 where 
    terminations = numberOfTerminations beginCase endCase 0 0 xs

hasNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
hasNestedCollapsible beginCase endCase xs = all (0 < ) [fst terminations, snd terminations] where
    terminations = numberOfTerminations beginCase endCase 0 0 xs

takeNest :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeNest _ _ [] = []
takeNest beginCase endCase (x:xs)
    | not (hNC (x:xs))  = []
    | not (iCNC (x:xs)) = takeNest beginCase endCase xs
    | otherwise         = takeUntilTerminationLevel endCase (snd (numberOfTerminations beginCase endCase 0 0 (x:xs))) (dropWhile (\xx -> not (beginCase xx)) (x:xs))
    where
        hNC ncs = hasNestedCollapsible beginCase endCase ncs
        iCNC ncs = isCompleteNestedCollapsible beginCase endCase ncs

takeUntilTerminationLevel :: (a -> Bool) -> Int -> [a] -> [a]
takeUntilTerminationLevel _ 0 _ = []
takeUntilTerminationLevel termCase termLevel (x:xs)
    | null xs = [] --should hopefully never reach this
    | termCase x = x : takeUntilTerminationLevel termCase (termLevel - 1) xs
    | otherwise  = x : takeUntilTerminationLevel termCase termLevel xs

numberOfTerminations :: (a -> Bool) -> (a -> Bool) -> Int -> Int -> [a] -> (Int,Int)
numberOfTerminations _ _ begins ends [] = (begins, ends)
numberOfTerminations beginCase endCase begins ends (x:xs)
    | beginCase x = numberOfTerminations beginCase endCase (begins + 1) ends       xs
    | endCase   x = numberOfTerminations beginCase endCase begins       (ends + 1) xs
    | otherwise   = numberOfTerminations beginCase endCase begins       ends       xs