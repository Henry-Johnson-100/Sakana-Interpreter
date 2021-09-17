module Token.Util.NestedCollapsible (
    NestPartition(..),
    unwrapPartition,
    NCCase(..),
    isCompleteNestedCollapsible,
    hasNestedCollapsible,
    hasDeeperNest,
    nestedCollapsibleIsPrefixOf,
    dropWhileList,
    takeWhileList,
    takeNestSameDepth,
    takeNestDeeper,
    takeNestFirstComplete,
    breakByNest,
    getNestedCollapsibleContents
) where


import Data.List
import Token.Util.EagerCollapsible (dropInfix)


data NestPartition a = NestPartition {
    partFst :: [a],
    partSnd :: [a],
    partThd :: [a]
} deriving (Show, Read, Eq)


unwrapPartition :: NestPartition a -> [[a]]
unwrapPartition (NestPartition f s t) = [f, s, t]


data NCCase a = NCCase {
    beginCase :: (a -> Bool),
    endCase   :: (a -> Bool)
}

tnc = NCCase ('('==) (')'==)
ts = "some (a,b) test (cd(ef(gh)))"

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


hasDeeperNest :: NCCase a -> [a] -> Bool
hasDeeperNest _ [] = False
hasDeeperNest nCCase xs = hasNestedCollapsible nCCase (tail xs)


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


dropWhileList :: ([a] -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList f xs
    | f xs = xs
    | otherwise = dropWhileList f (tail xs)


takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList _ [] = []
takeWhileList f (x:xs)
    | f (x:xs) = []
    | otherwise = x : (takeWhileList f xs)


takeNestSameDepth :: (Eq a) => NCCase a -> [a] -> [a]
takeNestSameDepth _ [] = []
takeNestSameDepth nCCase xs
    | not (hasNestedCollapsible nCCase xs)  = []
    | nestedCollapsibleIsPrefixOf nCCase xs = takeNestSameDepth nCCase (dropInfix (takeNestFirstComplete nCCase xs) xs)
    | otherwise                             = takeNestFirstComplete nCCase xs


takeNestDeeper :: NCCase a -> [a] -> [a]
takeNestDeeper _ [] = []
takeNestDeeper nCCase xs
    | not (hasNestedCollapsible nCCase xs)                             = []
    | nestedCollapsibleIsPrefixOf nCCase xs && hasDeeperNest nCCase xs = takeNestFirstComplete nCCase (tail xs)
    | otherwise                                                        = takeNestFirstComplete nCCase xs


takeNestFirstComplete :: NCCase a -> [a] -> [a] --Whatever the very first complete nest encountered by this function is, it will return that one
takeNestFirstComplete _ [] = []
takeNestFirstComplete nCCase xs
    | isCompleteNestedCollapsible nCCase xs = xs
    | not (hasNestedCollapsible nCCase xs)  = []
    | nestedCollapsibleIsPrefixOf nCCase xs = takeNestFirstComplete' nCCase 0 xs
    | otherwise                             = takeNestFirstComplete nCCase (tail xs)
    where
        takeNestFirstComplete' :: NCCase a -> Int -> [a] -> [a]
        takeNestFirstComplete' _ _ [] = []
        takeNestFirstComplete' nCCase' terminations (x:xs)
            | (beginCase nCCase') x = x : takeNestFirstComplete' nCCase' (terminations + 1) xs
            | (endCase nCCase')   x = case (terminations - 1) <= 0 of True  -> [x] -- <= here is weird and I'm not sure about it
                                                                      False -> x : takeNestFirstComplete' nCCase' (terminations - 1) xs
            | otherwise             = x : takeNestFirstComplete' nCCase' terminations xs


getNestedCollapsibleContents :: NCCase a -> [a] -> [a]
getNestedCollapsibleContents _ [] = []
getNestedCollapsibleContents nc xs
    | isCompleteNestedCollapsible nc xs = tail $ init xs
    | otherwise = xs


breakByNest :: (Eq a) => NCCase a -> [a] -> NestPartition a --bBN law => partFst ++ partSnd ++ partThd == xs ALWAYS
breakByNest _ [] = NestPartition [] [] []
breakByNest nCCase xs = NestPartition first second third where
    first  = takeWhileList (\xs' -> nestedCollapsibleIsPrefixOf nCCase xs') xs
    second = takeNestFirstComplete nCCase xs
    third  = dropInfix (first ++ second) xs


numberOfTerminations :: NCCase a -> [a] -> (Int, Int)
numberOfTerminations ncCase xs = numberOfTerminations' ncCase 0 0 xs where
    numberOfTerminations' :: NCCase a -> Int -> Int -> [a] -> (Int,Int)
    numberOfTerminations' _ begins ends [] = (begins, ends)
    numberOfTerminations' nCCase' begins ends (x:xs)
        | (beginCase nCCase') x = numberOfTerminations' nCCase' (begins + 1) ends       xs
        | (endCase nCCase')   x = numberOfTerminations' nCCase' begins       (ends + 1) xs
        | otherwise             = numberOfTerminations' nCCase' begins       ends       xs