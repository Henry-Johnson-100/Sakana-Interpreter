module Token.Data (
    Data(..),
    consolidateStrings,
    readData,
    fromData
) where


import Data.Char
import Data.List
import Token.Util.Like
import Token.Util.EagerCollapsible


data Data = Int Int | Float Float | String String | Boolean Bool | Id String | Punct String | Other String deriving (Show,Read,Eq,Ord)


instance Like Data where
    (Int a)     `like`    (Int b)     = True
    (Float _)   `like`    (Float _)   = True
    (String a)  `like`    (String b)  = True
    (Boolean a) `like`    (Boolean b) = True
    (Id _)      `like`    (Id _)      = True
    (Punct _)   `like`    (Punct _)   = True
    (Other a)   `like`    (Other b)   = True
    _           `like`    _           = False
    a           `notLike` b           = not $ like a b


fromData :: Data -> String
fromData (String a)  = a
fromData (Int a)     = show a
fromData (Float a)   = show a
fromData (Boolean a) = show a
fromData (Id a)      = a
fromData (Punct a)   = a
fromData (Other a)   = a


convertToDataString :: Data -> Data
convertToDataString d = String (fromData d)


mapToString :: [Data] -> [Data]
mapToString xs = map (convertToDataString) xs


strip :: String -> String
strip str = reverse $ dropWhile (isSpace) $ reverse $ dropWhile (isSpace) str


allDigits :: String -> Bool
allDigits str = all (isDigit) str


allPunct :: String -> Bool
allPunct str = all (isPunctuation) str


allAlphaNum :: String -> Bool
allAlphaNum str = (any (isDigit) str && any (isAlpha) str) && all (isAlphaNum) str


allAlpha :: String -> Bool
allAlpha str = all (isAlpha) str


isFloatStr :: String -> Bool
isFloatStr str = (elem '.' str) && (allDigits (filter ('.' /=) str))


couldBeId :: String -> Bool
couldBeId str = maybeContainsSnakeCaseOrDot && isOtherWiseAllAlpha && containsNoDigits
    where
        maybeContainsSnakeCaseOrDot = ((elem '.' str) || (elem '_' str)) || (allAlpha str)
        isOtherWiseAllAlpha         = (allAlpha (filter (\x -> ('.' /= x) && ('_' /= x)) str))
        containsNoDigits            = not $ any (isDigit) str


isStringPrefix :: Data -> Bool
isStringPrefix (String a) = (isPrefixOf "\"" a) && (not $ isSuffixOf "\"" a)
isStringPrefix _          = False


isStringSuffix :: Data -> Bool
isStringSuffix (String a) = (isSuffixOf "\"" a) && (not $ isPrefixOf "\"" a)
isStringSuffix _          = False


consolidateStrings :: [Data] -> [Data]
consolidateStrings [] = []
consolidateStrings (d:ds)
    | isStringPrefix d && isEagerCollapsible isStringPrefix isStringSuffix (d:ds) = (mapToConsolidatedStringData (d:ds)) ++ consolidateStrings (dropBetween isStringPrefix isStringSuffix (d:ds))
    | otherwise                                                                   = d : consolidateStrings ds
    where
        mapTakeBetween xs = mapToString $ takeBetween isStringPrefix isStringSuffix xs
        mapToConsolidatedStringData xs = String (concat ( map (fromData) (mapTakeBetween xs))) : []


readData :: String -> Data
readData pstr
    | null str                        = Other ""
    | allAlphaNum $ strip str                 = Other str
    | allDigits $ strip str                   = Int (read str :: Int)
    | isFloatStr $ strip str                  = Float (read str :: Float)
    | allPunct $ strip str                    = Punct str
    | elem '\"' str                   = String str --Don't like this one
    | str == "True" || str == "False" = Boolean ( read str :: Bool )
    | couldBeId $ strip str                   = Id str
    | otherwise                       = Other str
    where
        str = strip pstr