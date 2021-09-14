module Token.Util.NestedCollapsible (
    isCompleteNestedCollapsible,
    hasNestedCollapsible,
    takeNest,
    takeDeepestNest,
    takeSameDepth
) where


import Data.List
import Token.Util.EagerCollapsible (dropInfix)


isCompleteNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isCompleteNestedCollapsible beginCase endCase xs = isCompleteNestedCollapsible' beginCase endCase 0 0 xs

isCompleteNestedCollapsible' :: (a -> Bool) -> (a -> Bool) -> Int -> Int -> [a] -> Bool
isCompleteNestedCollapsible' _ _ begins ends [] = begins == ends && all (0 < ) [begins, ends]
isCompleteNestedCollapsible' beginCase endCase begins ends (x:xs)
    | endCase == beginCase && beginCase > 0 = False
    | beginCase x                           = isCompleteNestedCollapsible' beginCase endCase (begins + 1) ends xs
    | endCase x                             = isCompleteNestedCollapsible' beginCase endCase begins (ends + 1) xs
    | otherwise                             = isCompleteNestedCollapsible' beginCase endCase begins ends xs

hasNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
hasNestedCollapsible beginCase endCase xs = all (0 < ) [fst terminations, snd terminations] where
    terminations = numberOfTerminations beginCase endCase xs


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
        terminations = numberOfTerminations beginCase endCase (x:xs)
        minTuple (a, b) = minimum [a, b]


takeDeepestNest :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeDeepestNest beginCase endCase xs
    | null nextNest = xs
    | otherwise     = takeDeepestNest beginCase endCase nextNest
    where
        nextNest = takeNest beginCase endCase xs

takeUntilTerminationLevel :: (a -> Bool) -> Int -> [a] -> [a]
takeUntilTerminationLevel _ 0 _ = []
takeUntilTerminationLevel _ _ [] = []
takeUntilTerminationLevel termCase termLevel (x:xs)
    | termCase x = x : takeUntilTerminationLevel termCase (termLevel - 1) xs
    | otherwise  = x : takeUntilTerminationLevel termCase termLevel xs


takeSameDepth :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeSameDepth beginCase endCase xs
    | not (hasNestedCollapsible beginCase endCase xs) = []
    | not (beginCase (head xs))                       = takeNest beginCase endCase xs
    | otherwise                                       = takeSameDepth beginCase endCase (dropInfix (takeNest beginCase endCase xs) (xs))


getMaxNestedCollapsibleDepth :: (a -> Bool) -> (a -> Bool) -> [a] -> Int
getMaxNestedCollapsibleDepth beginCase endCase xs = minimum [fst depthTuple, snd depthTuple] where
    depthTuple = numberOfTerminations beginCase endCase xs


numberOfTerminations :: (a -> Bool) -> (a -> Bool) -> [a] -> (Int, Int)
numberOfTerminations beginCase endCase xs = numberOfTerminations' beginCase endCase 0 0 xs


numberOfTerminations' :: (a -> Bool) -> (a -> Bool) -> Int -> Int -> [a] -> (Int,Int)
numberOfTerminations' _ _ begins ends [] = (begins, ends)
numberOfTerminations' beginCase endCase begins ends (x:xs)
    | beginCase x = numberOfTerminations' beginCase endCase (begins + 1) ends       xs
    | endCase   x = numberOfTerminations' beginCase endCase begins       (ends + 1) xs
    | otherwise   = numberOfTerminations' beginCase endCase begins       ends       xs