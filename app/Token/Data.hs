module Token.Data (
    Data(..),
    consolidateEagerCollapsibleData,
    readData,
    fromData,
    punctRepr
) where


import Data.Char
import Data.List
import Token.Util.Like
import Token.Util.EagerCollapsible
import Token.Util.String (strip)


data Data = Int Int | Float Float | String String | Boolean Bool | Id String | Punct String | Other String | Comment String deriving (Show,Read,Eq,Ord)


instance Like Data where
    (Int _)     `like`    (Int _)     = True
    (Float _)   `like`    (Float _)   = True
    (String _)  `like`    (String _)  = True
    (Boolean _) `like`    (Boolean _) = True
    (Id _)      `like`    (Id _)      = True
    (Punct _)   `like`    (Punct _)   = True
    (Other _)   `like`    (Other _)   = True
    (Comment _) `like`    (Comment _) = True
    _           `like`    _           = False
    a           `notLike` b           = not $ like a b


punctRepr :: [String]
punctRepr = [","]


fromData :: Data -> String
fromData (String a)  = a
fromData (Int a)     = show a
fromData (Float a)   = show a
fromData (Boolean a) = show a
fromData (Id a)      = a
fromData (Punct a)   = a
fromData (Other a)   = a
fromData (Comment a) = a


allDigits :: String -> Bool
allDigits str = all (isDigit) str


allPunct :: String -> Bool
allPunct str = all (isPunctuation) str && str /= "\""


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
isStringPrefix (String a) = ((isPrefixOf "\"" a) && (not $ isSuffixOf "\"" a)) || (length a == 1)
isStringPrefix _          = False


isStringSuffix :: Data -> Bool
isStringSuffix (String a) = ((isSuffixOf "\"" a) && (not $ isPrefixOf "\"" a)) || (length a == 1)
isStringSuffix _          = False


isCommentPrefix :: Data -> Bool
isCommentPrefix (Comment a) = isPrefixOf "/*" a
isCommentPrefix _         = False


isCommentSuffix :: Data -> Bool
isCommentSuffix (Comment a) = isSuffixOf "*/" a
isCommentSuffix _         = False


consolidateEagerCollapsibleData :: [Data] -> [Data]
consolidateEagerCollapsibleData [] = []
consolidateEagerCollapsibleData (d:ds)
    | isStringPrefix  d && isEagerCollapsible isStringPrefix  isStringSuffix  (d:ds) = (mapToConsolidatedData (String "") (d:ds))  ++ consolidateEagerCollapsibleData (dropBetween (isStringPrefix)  (isStringSuffix)  (d:ds))
    | isCommentPrefix d && isEagerCollapsible isCommentPrefix isCommentSuffix (d:ds) = (mapToConsolidatedData (Comment "") (d:ds)) ++ consolidateEagerCollapsibleData (dropBetween (isCommentPrefix) (isCommentSuffix) (d:ds))
    | otherwise                                                                      = d : consolidateEagerCollapsibleData ds
    where
        mapTakeBetween :: Data -> [Data] -> [Data]
        mapTakeBetween emptyDataType xs = map (\d -> (getDataTypeConstructor emptyDataType) (fromData d)) $ takeBetween (isDataTypePrefix emptyDataType) (isDataTypeSuffix emptyDataType) xs
        mapToConsolidatedData :: Data -> [Data] -> [Data]
        mapToConsolidatedData emptyDataType xs = (getDataTypeConstructor emptyDataType) (concat (map (fromData) (mapTakeBetween emptyDataType xs))) : []
        isDataTypePrefix :: Data -> Data -> Bool
        isDataTypePrefix (String  _) = isStringPrefix
        isDataTypePrefix (Comment _) = isCommentPrefix
        isDataTypeSuffix :: Data -> Data -> Bool
        isDataTypeSuffix (String  _) = isStringSuffix
        isDataTypeSuffix (Comment _) = isCommentSuffix
        getDataTypeConstructor :: Data -> String -> Data
        getDataTypeConstructor (String  _) = String
        getDataTypeConstructor (Comment _) = Comment


readData :: String -> Data --These guards are order dependent which is annoying
readData paddedStr
    | null str                                   = Other ""
    | isPrefixOf "/*" str || isSuffixOf "*/" str = Comment str
    | allAlphaNum $ strip str                    = Other str
    | allDigits $ strip str                      = Int (read str :: Int)
    | isFloatStr $ strip str                     = Float (read str :: Float)
    | allPunct $ strip str                       = Punct str
    | elem '\"' str                              = String str --Don't like this one
    | str == "True" || str == "False"            = Boolean ( read str :: Bool )
    | couldBeId $ strip str                      = Id str
    | otherwise                                  = Other str
    where
        str = strip paddedStr