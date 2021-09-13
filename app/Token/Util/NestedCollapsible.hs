module Token.Util.NestedCollapsible (
    isCompleteNestedCollapsible
) where

import Data.List

isCompleteNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isCompleteNestedCollapsible beginCase endCase xs = (fst terminations) == (snd terminations) && (fst terminations) /= 0 where 
    terminations = numberOfTerminations beginCase endCase 0 0 xs

numberOfTerminations :: (a -> Bool) -> (a -> Bool) -> Int -> Int -> [a] -> (Int,Int)
numberOfTerminations _ _ begins ends [] = (begins, ends)
numberOfTerminations beginCase endCase begins ends (x:xs)
    | beginCase x = numberOfTerminations beginCase endCase (begins + 1) ends       xs
    | endCase   x = numberOfTerminations beginCase endCase begins       (ends + 1) xs
    | otherwise   = numberOfTerminations beginCase endCase begins       ends       xs