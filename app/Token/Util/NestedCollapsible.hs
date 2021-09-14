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
isCompleteNestedCollapsible beginCase endCase xs = isCompleteNestedCollapsible' beginCase endCase 0 0 xs && (beginCase (head xs)) && (endCase (last xs))
    where
        isCompleteNestedCollapsible' _ _ begins ends [] = begins == ends && all (0 < ) [begins, ends]
        isCompleteNestedCollapsible' beginCase endCase begins ends (x:xs)
            | ends == begins && begins > 0          = False
            | beginCase x                           = isCompleteNestedCollapsible' beginCase endCase (begins + 1) ends xs
            | endCase x                             = isCompleteNestedCollapsible' beginCase endCase begins (ends + 1) xs
            | otherwise                             = isCompleteNestedCollapsible' beginCase endCase begins ends xs


hasNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
hasNestedCollapsible beginCase endCase xs = all (0 < ) [fst terminations, snd terminations] where
    terminations = numberOfTerminations beginCase endCase xs


nestedCollapsibleIsPrefixOf :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
nestedCollapsibleIsPrefixOf _ _ [] = False
nestedCollapsibleIsPrefixOf beginCase endCase xs
    | not (beginCase (head xs)) = False
    | otherwise                 = nestedCollapsibleIsPrefixOf' beginCase endCase 0 xs
    where
        nestedCollapsibleIsPrefixOf' :: (a -> Bool) -> (a -> Bool) -> Int -> [a] -> Bool
        nestedCollapsibleIsPrefixOf' _ _ ignoreTerminations [] = ignoreTerminations == 0
        nestedCollapsibleIsPrefixOf' beginCase endCase ignoreTerminations (x:xs)
            | beginCase x = nestedCollapsibleIsPrefixOf' beginCase endCase (ignoreTerminations + 1) (xs)
            | endCase x   = case ((ignoreTerminations - 1) == 0) of True  -> True
                                                                    False -> nestedCollapsibleIsPrefixOf' beginCase endCase (ignoreTerminations - 1) xs
            | otherwise   = nestedCollapsibleIsPrefixOf' beginCase endCase ignoreTerminations xs


takeNest :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeNest _ _ [] = []
takeNest beginCase endCase xs
    | not (hasNC xs)                           = []
    | isNCPrefixed xs && not (isCompleteNC xs) = takeNest' beginCase endCase 0 xs
    | isCompleteNC xs && not (hasNC (tail xs)) = xs
    | isCompleteNC xs && hasNC (tail xs)       = takeNest beginCase endCase (tail xs)
    | otherwise                                = takeNest beginCase endCase (tail xs)
    where
        hasNC xs' = hasNestedCollapsible beginCase endCase xs'
        isCompleteNC xs' = isCompleteNestedCollapsible beginCase endCase xs'
        isNCPrefixed xs' = nestedCollapsibleIsPrefixOf beginCase endCase xs'
        takeNest' :: (a -> Bool) -> (a -> Bool) -> Int -> [a] -> [a]
        takeNest' _ _ _ [] = []
        takeNest' beginCase endCase terminations (x:xs)
            | beginCase x = x : takeNest' beginCase endCase (terminations + 1) xs
            | endCase   x = case (terminations - 1) <= 0 of True  -> [x] -- <= here is weird and I'm not sure about it
                                                            False -> x : takeNest' beginCase endCase (terminations - 1) xs
            | otherwise   = x : takeNest' beginCase endCase terminations xs


takeDeepestNest :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeDeepestNest beginCase endCase xs
    | xs == nextNest = xs
    | otherwise      = takeDeepestNest beginCase endCase nextNest
    where
        nextNest = takeNest beginCase endCase xs


takeSameDepth :: (Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeSameDepth beginCase endCase xs
    | not (hasNestedCollapsible beginCase endCase xs) = []
    | not (beginCase (head xs))                       = takeNest beginCase endCase xs
    | otherwise                                       = takeSameDepth beginCase endCase (dropInfix (takeNest beginCase endCase xs) (xs))


getMaxNestedCollapsibleDepth :: (a -> Bool) -> (a -> Bool) -> [a] -> Int
getMaxNestedCollapsibleDepth beginCase endCase xs = minimum [fst depthTuple, snd depthTuple] where
    depthTuple = numberOfTerminations beginCase endCase xs


numberOfTerminations :: (a -> Bool) -> (a -> Bool) -> [a] -> (Int, Int)
numberOfTerminations beginCase endCase xs = numberOfTerminations' beginCase endCase 0 0 xs where
    numberOfTerminations' :: (a -> Bool) -> (a -> Bool) -> Int -> Int -> [a] -> (Int,Int)
    numberOfTerminations' _ _ begins ends [] = (begins, ends)
    numberOfTerminations' beginCase endCase begins ends (x:xs)
        | beginCase x = numberOfTerminations' beginCase endCase (begins + 1) ends       xs
        | endCase   x = numberOfTerminations' beginCase endCase begins       (ends + 1) xs
        | otherwise   = numberOfTerminations' beginCase endCase begins       ends       xs