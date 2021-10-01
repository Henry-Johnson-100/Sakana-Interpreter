module Token.Util.NestedCollapsible
  ( TriplePartition (..),
    NCCase (..),
    unwrapPartition,
    isCompleteNestedCollapsible,
    hasNestedCollapsible,
    hasDeeperNest,
    nestedCollapsibleIsPrefixOf,
    dropWhileList,
    takeWhileList,
    takeNestSameDepth,
    takeNestDeeper,
    takeNestFirstComplete,
    takeNestWhileComplete,
    breakByNest,
    getNestedCollapsibleContents,
    split,
    splitOn,
    splitTopLevelNCOn,
    groupAllTopLevelNestedCollapsibles,
    groupTopLevelByNestedCollapsiblePartition,
    groupByPartition,
  )
where

import qualified Token.Util.EagerCollapsible as EagerCollapsible

data TriplePartition a = TriplePartition
  { partFst :: [a],
    partSnd :: [a],
    partThd :: [a]
  }
  deriving (Show, Read, Eq)

unwrapPartition :: TriplePartition a -> [[a]]
unwrapPartition (TriplePartition f s t) = [f, s, t]

data NCCase a = NCCase
  { beginCase :: a -> Bool,
    endCase :: a -> Bool
  }

isCompleteNestedCollapsible :: NCCase a -> [a] -> Bool
isCompleteNestedCollapsible nCCase xs =
  isCompleteNestedCollapsible' nCCase 0 0 xs
    && beginCase nCCase (head xs)
    && endCase nCCase (last xs)
  where
    isCompleteNestedCollapsible' _ begins ends [] =
      begins == ends && all (0 <) [begins, ends]
    isCompleteNestedCollapsible' nCCase' begins ends (x' : xs')
      | ends == begins && begins > 0 = False
      | beginCase nCCase' x' = isCompleteNestedCollapsible' nCCase' (begins + 1) ends xs'
      | endCase nCCase' x' = isCompleteNestedCollapsible' nCCase' begins (ends + 1) xs'
      | otherwise = isCompleteNestedCollapsible' nCCase' begins ends xs'

hasNestedCollapsible :: NCCase a -> [a] -> Bool
hasNestedCollapsible nCCase xs = all (0 <) [fst terminations, snd terminations]
  where
    terminations = numberOfTerminations nCCase xs

hasDeeperNest :: NCCase a -> [a] -> Bool
hasDeeperNest _ [] = False
hasDeeperNest nCCase xs = hasNestedCollapsible nCCase (tail xs)

nestedCollapsibleIsPrefixOf :: NCCase a -> [a] -> Bool
nestedCollapsibleIsPrefixOf _ [] = False
nestedCollapsibleIsPrefixOf nCCase xs
  | not (beginCase nCCase (head xs)) = False
  | otherwise = nestedCollapsibleIsPrefixOf' nCCase 0 xs
  where
    nestedCollapsibleIsPrefixOf' :: NCCase a -> Int -> [a] -> Bool
    nestedCollapsibleIsPrefixOf' _ ignoreTerminations [] = ignoreTerminations == 0
    nestedCollapsibleIsPrefixOf' nCCase' ignoreTerminations (x : xs)
      | beginCase nCCase' x =
        nestedCollapsibleIsPrefixOf' nCCase' (ignoreTerminations + 1) xs
      | endCase nCCase' x =
        ((ignoreTerminations - 1) == 0)
          || nestedCollapsibleIsPrefixOf' nCCase' (ignoreTerminations - 1) xs
      | otherwise = nestedCollapsibleIsPrefixOf' nCCase' ignoreTerminations xs

dropWhileList :: ([a] -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList f xs
  | f xs = dropWhileList f (tail xs)
  | otherwise = xs

takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList _ [] = []
takeWhileList f (x : xs)
  | f (x : xs) = x : takeWhileList f xs
  | otherwise = []

split :: (Eq a) => [a] -> a -> [[a]]
split [] _ = [[]]
split xs splitOn =
  filter (not . null) $
    takeWhile (splitOn /=) xs :
    split (tail' (dropWhile (splitOn /=) xs)) splitOn
  where
    tail' [] = []
    tail' (x : xs) = xs

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn splitF xs =
  filter (not . null) $
    takeWhile (not . splitF) xs :
    splitOn splitF (tail' (dropWhile (not . splitF) xs))
  where
    tail' [] = []
    tail' (x : xs) = xs

splitTopLevelNCOn :: (Eq a) => NCCase a -> (a -> Bool) -> [a] -> [[a]]
splitTopLevelNCOn nc splitF xs = filter (not . null) $ splitTopLevelNCOn' nc splitF xs
  where
    splitTopLevelNCOn' :: (Eq a) => NCCase a -> (a -> Bool) -> [a] -> [[a]]
    splitTopLevelNCOn' _ _ [] = [[]]
    splitTopLevelNCOn' nc splitF xs
      | null (partFst part) = partSnd part : splitTopLevelNCOn' nc splitF (partThd part)
      | splitF (last (partFst part)) =
        ( splitOn splitF (partFst part)
            ++ [partSnd part]
        )
          ++ splitTopLevelNCOn' nc splitF (partThd part)
      | otherwise =
        ( init (splitOn splitF (partFst part))
            ++ [last (splitOn splitF (partFst part)) ++ partSnd part]
        )
          ++ splitTopLevelNCOn' nc splitF (partThd part)
      where
        part = breakByNest nc xs

takeNestSameDepth :: (Eq a) => NCCase a -> [a] -> [a]
takeNestSameDepth _ [] = []
takeNestSameDepth nCCase xs
  | not (hasNestedCollapsible nCCase xs) = []
  | nestedCollapsibleIsPrefixOf nCCase xs =
    takeNestSameDepth
      nCCase
      (EagerCollapsible.dropInfix (takeNestFirstComplete nCCase xs) xs)
  | otherwise = takeNestFirstComplete nCCase xs

takeNestDeeper :: NCCase a -> [a] -> [a]
takeNestDeeper _ [] = []
takeNestDeeper nCCase xs
  | not (hasNestedCollapsible nCCase xs) = []
  | nestedCollapsibleIsPrefixOf nCCase xs && hasDeeperNest nCCase xs =
    takeNestFirstComplete nCCase (tail xs)
  | otherwise = takeNestFirstComplete nCCase xs

-- Whatever the very first complete nest encountered by this function is,
-- it will return that one
takeNestFirstComplete :: NCCase a -> [a] -> [a]
takeNestFirstComplete _ [] = []
takeNestFirstComplete nCCase xs
  | isCompleteNestedCollapsible nCCase xs = xs
  | not (hasNestedCollapsible nCCase xs) = []
  | nestedCollapsibleIsPrefixOf nCCase xs = takeNestFirstComplete' nCCase 0 xs
  | otherwise = takeNestFirstComplete nCCase (tail xs)
  where
    takeNestFirstComplete' :: NCCase a -> Int -> [a] -> [a]
    takeNestFirstComplete' _ _ [] = []
    takeNestFirstComplete' nCCase' terminations (x : xs)
      | beginCase nCCase' x = x : takeNestFirstComplete' nCCase' (terminations + 1) xs
      | endCase nCCase' x =
        if (terminations - 1) <= 0
          then [x]
          else x : takeNestFirstComplete' nCCase' (terminations - 1) xs
      | otherwise = x : takeNestFirstComplete' nCCase' terminations xs

takeNestWhileComplete :: NCCase a -> [a] -> [a]
takeNestWhileComplete _ [] = []
takeNestWhileComplete nc xs
  | isCompleteNestedCollapsible nc xs =
    takeNestWhileComplete nc (getNestedCollapsibleContents nc xs)
  | otherwise = xs

getNestedCollapsibleContents :: NCCase a -> [a] -> [a]
getNestedCollapsibleContents _ [] = []
getNestedCollapsibleContents nc xs
  | isCompleteNestedCollapsible nc xs = tail $ init xs
  | otherwise = xs

-- bBN law => partFst ++ partSnd ++ partThd == xs ALWAYS
breakByNest :: (Eq a) => NCCase a -> [a] -> TriplePartition a
breakByNest _ [] = TriplePartition [] [] []
breakByNest nCCase xs = TriplePartition first second third
  where
    first = takeWhileList (not . nestedCollapsibleIsPrefixOf nCCase) xs
    second = takeNestFirstComplete nCCase xs
    third = EagerCollapsible.dropInfix (first ++ second) xs

-- | Only groups the collapsibles
-- and not any free components not contained outside of them
groupAllTopLevelNestedCollapsibles :: (Eq a) => NCCase a -> [a] -> [[a]]
groupAllTopLevelNestedCollapsibles _ [] = []
groupAllTopLevelNestedCollapsibles nc xs
  | null (partThd part) = [partSnd part]
  | otherwise = partSnd part : groupAllTopLevelNestedCollapsibles nc (partThd part)
  where
    part = breakByNest nc xs

groupTopLevelByNestedCollapsiblePartition :: (Eq a) => NCCase a -> [a] -> [[a]]
groupTopLevelByNestedCollapsiblePartition _ [] = []
groupTopLevelByNestedCollapsiblePartition nc xs
  | null (partThd part) = [partFst part, partSnd part]
  | otherwise =
    partFst part :
    partSnd part :
    groupTopLevelByNestedCollapsiblePartition nc (partThd part)
  where
    part = breakByNest nc xs

groupByPartition :: (Eq a) => NCCase a -> [a] -> [TriplePartition a]
groupByPartition _ [] = []
groupByPartition nc xs
  | (null . partThd) part = [TriplePartition (partFst part) (partSnd part) []]
  | otherwise =
    TriplePartition (partFst part) (partSnd part) [] : groupByPartition nc (partThd part)
  where
    part = breakByNest nc xs

numberOfTerminations :: NCCase a -> [a] -> (Int, Int)
numberOfTerminations ncCase xs = numberOfTerminations' ncCase 0 0 xs
  where
    numberOfTerminations' :: NCCase a -> Int -> Int -> [a] -> (Int, Int)
    numberOfTerminations' _ begins ends [] = (begins, ends)
    numberOfTerminations' nCCase' begins ends (x : xs)
      | beginCase nCCase' x = numberOfTerminations' nCCase' (begins + 1) ends xs
      | endCase nCCase' x = numberOfTerminations' nCCase' begins (ends + 1) xs
      | otherwise = numberOfTerminations' nCCase' begins ends xs