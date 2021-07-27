module Token.Data (
) where


import Data.Char
import Data.List
import Token.Util.Like
import Token.Util.EagerCollapsible


data Data = Int Int | String String | Boolean Bool | Other String deriving (Show,Read,Eq,Ord)


instance Like Data where
    (Int a)     `like`    (Int b)     = True
    (String a)  `like`    (String b)  = True
    (Boolean a) `like`    (Boolean b) = True
    (Other a)   `like`    (Other b)   = True
    _           `like`    _           = False
    a           `notLike` b           = not $ like a b


test = [Int 5, String "\"Hello ", Other "dumb ", String "world\"", Int 5, String "\"Another ", Other "dumb ", Other "consolidate ", String "test\"", Int 5 ]


fromData :: Data -> String
fromData (String a) = a
fromData (Int a) = show a
fromData (Boolean a) = show a
fromData (Other a) = a


convertToDataString :: Data -> Data
convertToDataString d = String (fromData d)


mapToString :: [Data] -> [Data]
mapToString xs = map (convertToDataString) xs


allDigits :: String -> Bool
allDigits str = all (isDigit) str


isStringPrefix :: Data -> Bool
isStringPrefix (String a) = isPrefixOf "\"" a
isStringPrefix _          = False


isStringSuffix :: Data -> Bool
isStringSuffix (String a) = isSuffixOf "\"" a
isStringSuffix _          = False


{---#FIXME-} --Doesn't work if the list contains two or more separate strings
consolidateStrings :: [Data] -> [Data]
consolidateStrings [] = []
consolidateStrings (d:ds)
    | isStringPrefix d && isEC ds = consolidatedString (d:ds) ++ (consolidateStrings (d:ds))
    | otherwise = d : consolidateStrings ds
    where 
        consolidatedString :: [Data] -> [Data]
        consolidatedString xs = mapToString (takeBetween (isStringPrefix) (isStringSuffix) xs)

        isEC :: [Data] -> Bool
        isEC xs = isEagerCollapsible isStringPrefix isStringSuffix xs


readData :: String -> Data
readData str
    | null str                        = Other ""
    | allDigits str                   = Int (read str :: Int)
    | elem '\"' str                   = String str --Don't like this one
    | str == "True" || str == "False" = Boolean ( read str :: Bool )
    | otherwise                       = Other str