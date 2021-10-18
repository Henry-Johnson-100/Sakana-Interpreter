module Token.Data
  ( Data (..),
    miscRepr,
    readData,
    fromData,
    isPrimitive,
    isNumeric,
    unNum,
    unString,
    unBoolean,
  )
where

import qualified Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation)
import qualified Data.List (isPrefixOf, isSuffixOf)
import qualified Token.Util.Like as LikeClass (Like (..))
import qualified Token.Util.String (strip)

data Data
  = Num Double
  | String String
  | Boolean Bool
  | Id String
  | Punct String
  | Other String
  | Comment String
  | Null
  deriving (Show, Read, Eq, Ord)

instance LikeClass.Like Data where
  (Num _) `like` (Num _) = True
  (String _) `like` (String _) = True
  (Boolean _) `like` (Boolean _) = True
  (Id _) `like` (Id _) = True
  (Punct _) `like` (Punct _) = True
  (Other _) `like` (Other _) = True
  (Comment _) `like` (Comment _) = True
  Null `like` Null = True
  _ `like` _ = False
  a `notLike` b = not $ LikeClass.like a b

miscRepr :: [String]
miscRepr = [",", "/*", "*/"]

fromData :: Data -> String
fromData (String a) = a
fromData (Num a) = show a
fromData (Boolean a) = show a
fromData (Id a) = a
fromData (Punct a) = a
fromData (Other a) = a
fromData (Comment a) = a
fromData Null = ""

allDigits :: String -> Bool
allDigits ('-' : xs) = allDigits xs
allDigits str = all Data.Char.isDigit str

allPunct :: String -> Bool
allPunct str = all Data.Char.isPunctuation str && str /= "\""

allAlphaNum :: String -> Bool
allAlphaNum str =
  (any Data.Char.isDigit str && any Data.Char.isAlpha str) && all Data.Char.isAlphaNum str

allAlpha :: String -> Bool
allAlpha = all Data.Char.isAlpha

isFloatStr :: String -> Bool
isFloatStr ('-' : xs) = isFloatStr xs
isFloatStr str = elem '.' str && allDigits (filter ('.' /=) str)

couldBeId :: String -> Bool
couldBeId str = maybeContainsSnakeCaseOrDot && isOtherWiseAllAlpha && containsNoDigits
  where
    maybeContainsSnakeCaseOrDot = (elem '.' str || elem '_' str) || allAlpha str
    isOtherWiseAllAlpha = allAlpha (filter (\x -> ('.' /= x) && ('_' /= x)) str)
    containsNoDigits = not $ any Data.Char.isDigit str

isPrimitive :: Data -> Bool
isPrimitive d = any (d `LikeClass.like`) [Num 0.0, String "", Boolean True, Null]

isNumeric :: Data -> Bool
isNumeric d = any (d `LikeClass.like`) [Num 0.0, Null]

unNum :: Data -> Maybe Double
unNum (Num x) = Just x
unNum Null = Just 0.0
unNum _ = Nothing

unString :: Data -> Maybe String
unString (String s) = Just s
unString Null = Just ""
unString _ = Nothing

unBoolean :: Data -> Maybe Bool
unBoolean (Boolean b) = Just b
unBoolean Null = Just False
unBoolean _ = Nothing

readData :: String -> Data --These guards are order dependent which is annoying
readData paddedStr
  | null str = Other ""
  | Data.List.isPrefixOf "/*" str || Data.List.isSuffixOf "*/" str = Comment str
  | allAlphaNum $ Token.Util.String.strip str = Other str
  | allDigits $ Token.Util.String.strip str = Num (read str)
  | isFloatStr $ Token.Util.String.strip str = Num (read str)
  | allPunct $ Token.Util.String.strip str = Punct str
  | '\"' `elem` str = String str --Don't like this one
  | str == "True" || str == "False" = Boolean (read str :: Bool)
  | couldBeId $ Token.Util.String.strip str = Id str
  | otherwise = Other str
  where
    str = Token.Util.String.strip paddedStr