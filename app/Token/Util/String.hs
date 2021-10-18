module Token.Util.String
  ( replaceAll,
    genSpace,
    padFront,
    padRear,
    padEqual,
    strip,
    onlyLiteral,
  )
where

import qualified Data.Char (isSpace)
import qualified Data.List (isPrefixOf, isSuffixOf)
import qualified Token.Util.EagerCollapsible as EagerCollapsible (dropInfix)

replaceAll :: String -> String -> String -> String
replaceAll (s : str) find replace
  | null str = s : ""
  | find `Data.List.isPrefixOf` (s : str) =
    replace ++ replaceAll (EagerCollapsible.dropInfix find (s : str)) find replace
  | otherwise = s : replaceAll str find replace

genSpace :: Int -> String
genSpace space = concat [" " | x <- [1 .. space]]

padFront :: String -> Int -> String
padFront str space = genSpace space ++ str

padRear :: String -> Int -> String
padRear str space = str ++ genSpace space

padEqual :: String -> Int -> String
padEqual str space = padRear (padFront str space) space

strip :: String -> String
strip str =
  reverse $
    dropWhile Data.Char.isSpace $
      reverse $
        dropWhile Data.Char.isSpace str

onlyLiteral :: String -> String
onlyLiteral strs
  | "\"" `Data.List.isPrefixOf` strs = (onlyLiteral . tail) strs
  | "\"" `Data.List.isSuffixOf` strs = (onlyLiteral . init) strs
  | otherwise = strs