module Token.Util.String(
    replaceAll
) where

import Data.List
import Token.Util.EagerCollapsible

replaceAll :: String -> String -> String -> String
replaceAll (s:str) find replace
    | null str = s : ""
    | isPrefixOf find (s:str) = replace ++ replaceAll (dropInfix find (s:str)) find replace
    | otherwise               = s : replaceAll str find replace