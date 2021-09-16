module Token.Util.NestedCollapsible (
    NestPartition(..),
    NCCase(..),
    unwrapPartition,
    isCompleteNestedCollapsible,
    hasNestedCollapsible,
    takeNest,
    takeDeepestNest,
    partitionNests
) where


import Data.List
import Token.Util.EagerCollapsible (dropInfix)


data NestPartition a = NestPartition {
    partFst :: [a],
    partSnd :: [a],
    partThd :: [a]
} deriving (Show, Read, Eq)


data NCCase a = NCCase {
    beginCase :: (a -> Bool),
    endCase   :: (a -> Bool)
}


unwrapPartition :: NestPartition a -> [[a]]
unwrapPartition part = [partFst part, partSnd part, partThd part]


isCompleteNestedCollapsible :: NCCase a -> [a] -> Bool
isCompleteNestedCollapsible nCCase xs = isCompleteNestedCollapsible' nCCase 0 0 xs && ((beginCase nCCase) (head xs)) && ((endCase nCCase) (last xs))
    where
        isCompleteNestedCollapsible' _ begins ends [] = begins == ends && all (0 < ) [begins, ends]
        isCompleteNestedCollapsible' nCCase' begins ends (x':xs')
            | ends == begins && begins > 0 = False
            | (beginCase nCCase') x'       = isCompleteNestedCollapsible' nCCase' (begins + 1) ends       xs'
            | (endCase nCCase') x'         = isCompleteNestedCollapsible' nCCase' begins       (ends + 1) xs'
            | otherwise                    = isCompleteNestedCollapsible' nCCase' begins       ends       xs'


hasNestedCollapsible :: NCCase a -> [a] -> Bool
hasNestedCollapsible nCCase xs = all (0 < ) [fst terminations, snd terminations] where
    terminations = numberOfTerminations nCCase xs


nestedCollapsibleIsPrefixOf :: NCCase a -> [a] -> Bool
nestedCollapsibleIsPrefixOf _ [] = False
nestedCollapsibleIsPrefixOf nCCase xs
    | not ((beginCase nCCase) (head xs)) = False
    | otherwise                 = nestedCollapsibleIsPrefixOf' nCCase 0 xs
    where
        nestedCollapsibleIsPrefixOf' :: NCCase a -> Int -> [a] -> Bool
        nestedCollapsibleIsPrefixOf' _ ignoreTerminations [] = ignoreTerminations == 0
        nestedCollapsibleIsPrefixOf' nCCase' ignoreTerminations (x:xs)
            | (beginCase nCCase') x = nestedCollapsibleIsPrefixOf' nCCase' (ignoreTerminations + 1) (xs)
            | (endCase nCCase')   x = case ((ignoreTerminations - 1) == 0) of True  -> True
                                                                              False -> nestedCollapsibleIsPrefixOf' nCCase' (ignoreTerminations - 1) xs
            | otherwise             = nestedCollapsibleIsPrefixOf' nCCase' ignoreTerminations xs


takeNest :: NCCase a -> [a] -> [a]
takeNest _ [] = []
takeNest nCCase xs
    | not (hasNC xs)                           = []
    | isNCPrefixed xs && not (isCompleteNC xs) = takeNest' nCCase 0 xs
    | isCompleteNC xs && not (hasNC (tail xs)) = xs
    | isCompleteNC xs && hasNC (tail xs)       = takeNest nCCase (tail xs) --This line is causing bugs -> " test (fg,(hi(jk)))" returns (hi(jk)) instead of the entire first nest
    | otherwise                                = takeNest nCCase (tail xs)
    where
        hasNC xs' = hasNestedCollapsible nCCase xs'
        isCompleteNC xs' = isCompleteNestedCollapsible nCCase xs'
        isNCPrefixed xs' = nestedCollapsibleIsPrefixOf nCCase xs'
        takeNest' :: NCCase a -> Int -> [a] -> [a]
        takeNest' _ _ [] = []
        takeNest' nCCase' terminations (x:xs)
            | (beginCase nCCase') x = x : takeNest' nCCase' (terminations + 1) xs
            | (endCase nCCase')   x = case (terminations - 1) <= 0 of True  -> [x] -- <= here is weird and I'm not sure about it
                                                                      False -> x : takeNest' nCCase' (terminations - 1) xs
            | otherwise             = x : takeNest' nCCase' terminations xs


takeDeepestNest :: (Eq a) => NCCase a -> [a] -> [a]
takeDeepestNest nCCase xs
    | xs == nextNest = xs
    | otherwise      = takeDeepestNest nCCase nextNest
    where
        nextNest = takeNest nCCase xs


getMaxNestedCollapsibleDepth :: NCCase a -> [a] -> Int
getMaxNestedCollapsibleDepth nCCase xs = minimum [fst depthTuple, snd depthTuple] where
    depthTuple = numberOfTerminations nCCase xs


numberOfTerminations :: NCCase a -> [a] -> (Int, Int)
numberOfTerminations ncCase xs = numberOfTerminations' ncCase 0 0 xs where
    numberOfTerminations' :: NCCase a -> Int -> Int -> [a] -> (Int,Int)
    numberOfTerminations' _ begins ends [] = (begins, ends)
    numberOfTerminations' nCCase' begins ends (x:xs)
        | (beginCase nCCase') x = numberOfTerminations' nCCase' (begins + 1) ends       xs
        | (endCase nCCase')   x = numberOfTerminations' nCCase' begins       (ends + 1) xs
        | otherwise             = numberOfTerminations' nCCase' begins       ends       xs


partitionNests :: (Eq a) => NCCase a -> [a] -> NestPartition a
partitionNests _ [] = NestPartition [] [] []
partitionNests nCCase xs = NestPartition (preNest xs) (nest xs) (postNest xs) where
    --preNest :: [a] -> [a]
    preNest [] = []
    preNest xs'
        | nestedCollapsibleIsPrefixOf nCCase xs' = []
        | otherwise                          = (head xs') : (preNest (tail xs'))
    --nest :: [a] -> [a]
    nest xs' = takeNest nCCase (dropInfix (preNest xs') xs')
    --postNest :: [a] -> [a]
    postNest xs' = dropInfix ((preNest xs') ++ (nest xs')) xs'

groupByNests :: (Eq a) => NCCase a -> [a] -> [[a]]
groupByNests _ [] = [[]]
groupByNests nCCase xs
    | isCompleteNestedCollapsible nCCase xs || not (hasNestedCollapsible nCCase xs) = [xs]
    | otherwise                                                                     = partFst part : partSnd part : (groupByNests nCCase (partThd part))
    where
        part = partitionNests nCCase xs