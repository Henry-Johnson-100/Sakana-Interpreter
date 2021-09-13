module Token.Util.NestedCollapsible (
) where

import Data.List

isNestedCollapsible :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
isNestedCollapsible beginCase endCase xs = foldIsNC beginCase endCase 0 0 xs

foldIsNC :: (a -> Bool) -> (a -> Bool) -> Int -> Int -> [a] -> Bool
foldIsNC _ _ begins ends [] = begins == ends && begins /= 0
foldIsNC beginCase endCase begins ends (x:xs)
    | ends > begins = False
    | beginCase x   = foldIsNC beginCase endCase (begins + 1) ends xs
    | endCase x     = foldIsNC beginCase endCase begins (ends + 1) xs