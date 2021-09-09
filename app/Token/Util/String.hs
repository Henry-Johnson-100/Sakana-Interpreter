module Token.Util.String(
    replaceAll,
    genSpace,
    padFront,
    padRear
) where

import Data.List
import Token.Util.EagerCollapsible

replaceAll :: String -> String -> String -> String
replaceAll (s:str) find replace
    | null str = s : ""
    | isPrefixOf find (s:str) = replace ++ replaceAll (dropInfix find (s:str)) find replace
    | otherwise               = s : replaceAll str find replace

genSpace :: Int -> String
genSpace space = concat [" " | x <- [1..space] ]

padFront :: String -> Int -> String
padFront str space = (genSpace space) ++ str

padRear :: String -> Int -> String
padRear str space = str ++ (genSpace space)