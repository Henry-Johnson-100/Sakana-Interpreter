module Token.Data
  ( Data (..),
    miscRepr,
    readData,
    fromData,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import Data.List (isPrefixOf, isSuffixOf)
import Token.Util.Like (Like (..))
import Token.Util.String (strip)

data Data = Int Int | Float Float | String String | Boolean Bool | Id String | Punct String | Other String | Comment String deriving (Show, Read, Eq, Ord)

instance Like Data where
  (Int _) `like` (Int _) = True
  (Float _) `like` (Float _) = True
  (String _) `like` (String _) = True
  (Boolean _) `like` (Boolean _) = True
  (Id _) `like` (Id _) = True
  (Punct _) `like` (Punct _) = True
  (Other _) `like` (Other _) = True
  (Comment _) `like` (Comment _) = True
  _ `like` _ = False
  a `notLike` b = not $ like a b

miscRepr :: [String]
miscRepr = [",", "/*", "*/"]

fromData :: Data -> String
fromData (String a) = a
fromData (Int a) = show a
fromData (Float a) = show a
fromData (Boolean a) = show a
fromData (Id a) = a
fromData (Punct a) = a
fromData (Other a) = a
fromData (Comment a) = a

allDigits :: String -> Bool
allDigits ('-' : xs) = allDigits xs
allDigits str = all isDigit str

allPunct :: String -> Bool
allPunct str = all isPunctuation str && str /= "\""

allAlphaNum :: String -> Bool
allAlphaNum str = (any isDigit str && any isAlpha str) && all isAlphaNum str

allAlpha :: String -> Bool
allAlpha = all isAlpha

isFloatStr :: String -> Bool
isFloatStr ('-' : xs) = isFloatStr xs
isFloatStr str = elem '.' str && allDigits (filter ('.' /=) str)

couldBeId :: String -> Bool
couldBeId str = maybeContainsSnakeCaseOrDot && isOtherWiseAllAlpha && containsNoDigits
  where
    maybeContainsSnakeCaseOrDot = (elem '.' str || elem '_' str) || allAlpha str
    isOtherWiseAllAlpha = allAlpha (filter (\x -> ('.' /= x) && ('_' /= x)) str)
    containsNoDigits = not $ any isDigit str

readData :: String -> Data --These guards are order dependent which is annoying
readData paddedStr
  | null str = Other ""
  | isPrefixOf "/*" str || isSuffixOf "*/" str = Comment str
  | allAlphaNum $ strip str = Other str
  | allDigits $ strip str = Int (read str :: Int)
  | isFloatStr $ strip str = Float (read str :: Float)
  | allPunct $ strip str = Punct str
  | '\"' `elem` str = String str --Don't like this one
  | str == "True" || str == "False" = Boolean (read str :: Bool)
  | couldBeId $ strip str = Id str
  | otherwise = Other str
  where
    str = strip paddedStr