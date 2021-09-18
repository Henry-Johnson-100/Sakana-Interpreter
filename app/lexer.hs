module Lexer (
    Token(..),
    tokenize,
    like,
    fromToken,
    filterLike,
    filterNotLike,
    baseBracket
) where

import Data.List
import Data.Char (isSpace)
import Token.Util.Like
import Token.Util.String
import Token.Util.EagerCollapsible
import Token.Util.NestedCollapsible
import qualified Token.Bracket    as B
import qualified Token.Control    as C
import qualified Token.Data       as D
import qualified Token.Keyword    as K
import qualified Token.Operator   as O

data Token = Bracket B.Bracket | Control C.Control | Data D.Data | Keyword K.Keyword | Operator O.Operator deriving (Show,Read,Eq)

data TokenInLine = TokenInLine {
    token :: Token,
    line  :: Int
} deriving (Show,Read,Eq)


instance Like TokenInLine where
    like x y = (token x) `like` (token y)
    notLike x y = not $ like x y


instance Like Token where
    like (Bracket a)       (Bracket b)  = True
    like (Control a)       (Control b)  = True
    like (Data a)          (Data b)     = True
    like (Keyword a)       (Keyword b)  = True
    like (Operator a)      (Operator b) = True
    like _                 _            = False
    notLike a              b            = not $ like a b


baseBracket :: Token -> B.Bracket
baseBracket (Bracket b) = b

baseControl :: Token -> C.Control
baseControl (Lexer.Control c) = c

baseData :: Token -> D.Data
baseData (Data d) = d

tokenFromData :: D.Data -> Token
tokenFromData d = Data d

baseKeyword :: Token -> K.Keyword
baseKeyword (Keyword k) = k

baseOperator :: Token -> O.Operator
baseOperator (Operator o) = o

filterLike :: Token -> [Token] -> [Token]
filterLike t ts = filter (\x -> x `like` t) ts

filterNotLike :: Token -> [Token] -> [Token]
filterNotLike t ts = filter (\x -> x `notLike` t) ts

fromToken :: Token -> String
fromToken (Bracket bracket)       = B.fromBracket bracket
fromToken (Lexer.Control control) = C.fromControl control
fromToken (Data d)                = D.fromData    d
fromToken (Keyword keyword)       = K.fromKeyword keyword
fromToken (Operator operator)     = O.fromOp      operator

readToken :: String -> Token
readToken str
    | elem str K.repr = Keyword       (K.readKeyword str)
    | elem str B.repr = Bracket       (B.readBracket str)
    | elem str C.repr = Lexer.Control (C.readControl str)
    | elem str O.repr = Operator      (O.readOp      str)
    | otherwise       = Data          (D.readData    str)


addSpaces :: String -> String
addSpaces str
    | null str = ""
    | isAnyReprInHeadGroup B.repr     = (padReprElemFromHeadGroup B.repr 1)     ++ (addSpaces $ dropReprElemFromHeadGroup B.repr str)
    | isAnyReprInHeadGroup D.miscRepr = (padReprElemFromHeadGroup D.miscRepr 1) ++ (addSpaces $ dropReprElemFromHeadGroup D.miscRepr str)
    | isAnyReprInHeadGroup O.repr     = case length (filterReprElemsInHeadGroup O.repr) == 1 of True  -> (padReprElemFromHeadGroup O.repr 1)                                         ++ (addSpaces $ dropReprElemFromHeadGroup O.repr str)
                                                                                                False -> (padEqual (getLongestStringFromList (filterReprElemsInHeadGroup O.repr)) 1) ++ (addSpaces $ drop (maximum (map (length) (filterReprElemsInHeadGroup O.repr))) str )
    | otherwise = (head str) : addSpaces (tail str)
    where
        headGroup :: String
        headGroup = take 5 str
        isAnyReprInHeadGroup :: [String] -> Bool
        isAnyReprInHeadGroup reprList = any (\reprElem -> isPrefixOf reprElem headGroup) reprList
        filterReprElemsInHeadGroup :: [String] -> [String]
        filterReprElemsInHeadGroup reprList = filter (\reprElem -> isPrefixOf reprElem headGroup) reprList
        getReprElemInHeadGroup :: [String] -> String
        getReprElemInHeadGroup reprList = head $ filterReprElemsInHeadGroup reprList
        padReprElemFromHeadGroup :: [String] -> Int -> String
        padReprElemFromHeadGroup reprList space = padEqual (getReprElemInHeadGroup reprList) space
        dropReprElemFromHeadGroup :: [String] -> String -> String
        dropReprElemFromHeadGroup reprList str = drop (length (getReprElemInHeadGroup reprList)) str
        getLongestStringFromList :: [String] -> String
        getLongestStringFromList strs = head $ filter (\x -> length x == maximum (map length strs)) strs


isStringPrefix :: Token -> Bool
isStringPrefix (Data (D.String a)) = ((isPrefixOf "\"" a) && (not $ isSuffixOf "\"" a)) || (length a == 1)
isStringPrefix _                   = False


isStringSuffix :: Token -> Bool
isStringSuffix (Data (D.String a)) = ((isSuffixOf "\"" a) && (not $ isPrefixOf "\"" a)) || (length a == 1)
isStringSuffix _          = False


isCommentPrefix :: Token -> Bool
isCommentPrefix (Data (D.Comment a)) = isPrefixOf "/*" a
isCommentPrefix _         = False


isCommentSuffix :: Token -> Bool
isCommentSuffix (Data (D.Comment a)) = isSuffixOf "*/" a
isCommentSuffix _         = False


consolidateEagerCollapsibleTokens :: [Token] -> [Token]
consolidateEagerCollapsibleTokens [] = []
consolidateEagerCollapsibleTokens (t:ts)
    | isStringPrefix  t && isEagerCollapsible isStringPrefix  isStringSuffix  (t:ts) = (mapToConsolidatedData (Data (D.String "")) (t:ts))  ++ consolidateEagerCollapsibleTokens (dropBetween (isStringPrefix)  (isStringSuffix)  (t:ts))
    | otherwise                                                                      = t : consolidateEagerCollapsibleTokens ts
    where
        mapTakeBetween :: Token -> [Token] -> [Token]
        mapTakeBetween emptyTokenDataType xs = map (\t -> (constructDataToken emptyTokenDataType) (D.fromData (baseData t))) $ takeBetween (isDataTypePrefix emptyTokenDataType) (isDataTypeSuffix emptyTokenDataType) xs
        mapToConsolidatedData :: Token -> [Token] -> [Token]
        mapToConsolidatedData emptyTokenDataType xs = (constructDataToken emptyTokenDataType) (concat (map (\t -> D.fromData (baseData t)) (mapTakeBetween emptyTokenDataType xs))) : []
        isDataTypePrefix :: Token -> Token -> Bool
        isDataTypePrefix (Data (D.String  _)) = isStringPrefix
        isDataTypeSuffix :: Token -> Token -> Bool
        isDataTypeSuffix (Data (D.String  _)) = isStringSuffix
        constructDataToken :: Token -> String -> Token
        constructDataToken (Data (D.String  _)) str = (Data (D.String str))


mapOnTokenInLine :: (Token -> Token) -> [TokenInLine] -> [TokenInLine]
mapOnTokenInLine _ [] = []
mapOnTokenInLine f (t:tils) = (TokenInLine (f (token t)) (line t)) : mapOnTokenInLine f tils


consolidateEagerCollapsibleTokenInLines :: [TokenInLine] -> [TokenInLine]
consolidateEagerCollapsibleTokenInLines [] = []
consolidateEagerCollapsibleTokenInLines (t:ts)
    | isStringPrefix (token t) && isEagerCollapsible (\t' -> isStringPrefix (token t'))  (\t' -> isStringSuffix (token t'))  (t:ts) = (mapToConsolidatedDataString (t:ts))  ++ consolidateEagerCollapsibleTokenInLines (dropBetween (\t' -> isStringPrefix (token t'))  (\t' -> isStringSuffix (token t'))  (t:ts))
    | otherwise                                                                                                                     = t : consolidateEagerCollapsibleTokenInLines ts
    where
        mapTakeBetween :: [TokenInLine] -> [TokenInLine]
        mapTakeBetween xs = mapOnTokenInLine (\t -> (Data (D.String (D.fromData (baseData t))))) $ takeBetween (\t -> isStringPrefix (token t)) (\t -> isStringSuffix (token t)) xs
        mapToConsolidatedDataString :: [TokenInLine] -> [TokenInLine]
        mapToConsolidatedDataString xs = TokenInLine (Data (D.String (concat (mapOnTokenInLine (\t -> D.fromData (baseData t)) (mapTakeBetween xs))))) (line (head xs))


consolidateNestedCollapsibleTokens :: [Token] -> [Token]
consolidateNestedCollapsibleTokens [] = []
consolidateNestedCollapsibleTokens ts 
    | all null (unwrapPartition part) = ts
    | otherwise =  (flattenedFstSnd part) ++ (consolidateNestedCollapsibleTokens (partThd part))
    where
        part = breakByNest (NCCase isCommentPrefix isCommentSuffix) ts
        flattenedFstSnd part' = (partFst part') ++ ((Data (D.Comment (concat (map (fromToken) (partSnd part'))))) : [])


intersperseSpaceOtherTypes :: [Token] -> [Token]
intersperseSpaceOtherTypes [] = []
intersperseSpaceOtherTypes (t:ts)
    | isStringPrefix t || isStringSuffix t = t : (head ts)  : intersperseSpaceOtherTypes (tail ts)
    | otherwise                            = t : spaceOtherType : intersperseSpaceOtherTypes ts
    where spaceOtherType = Data (D.Other " ")


removeSpaceOtherTypes :: [Token] -> [Token]
removeSpaceOtherTypes [] = []
removeSpaceOtherTypes ts = filter ((/=) (Data (D.Other " "))) ts


tokenIsComment :: Token -> Bool
tokenIsComment t = t `like` (Data (D.Other "")) && (baseData t) `like` (D.Comment "")


ignoreComments :: [Token] -> [Token]
ignoreComments [] = []
ignoreComments ts = filter (\t -> not (tokenIsComment t)) ts


wordsPreserveStringSpacing :: [String] -> String -> [String]
wordsPreserveStringSpacing strs "" = strs
wordsPreserveStringSpacing strs (s:str)
    | s == '"' = wordsPreserveStringSpacing (strs ++ ((buildPreservedString str) : [])) (dropInfix (buildPreservedString str) (s:str))
    | isSpace s = wordsPreserveStringSpacing strs str
    | otherwise = wordsPreserveStringSpacing (strs ++ ((buildWord (s:str)) : [])) (dropInfix (buildWord (s:str)) (s:str))
    where
        buildPreservedString :: String -> String
        buildPreservedString str = "\"" ++ (takeWhile ((/=) '\"') str ) ++ "\""
        buildWord            :: String -> String
        buildWord str = (takeWhile (\s -> not (isSpace s)) str)


-- tokenize :: String -> [Token]
-- tokenize strs = ignoreComments $ consolidateNestedCollapsibleTokens $ consolidateEagerCollapsibleTokens $ map (readToken) $ wordsPreserveStringSpacing [] $ addSpaces strs

--tokenize :: String -> [TokenInLine]
-- tokenize strs = do
--     zippedRawCodeAndLines <- zip ([1..((length linesStrs) + 1)])                    (linesStrs) 
--     rawCodeAndLines       <- mapPreserveLn addSpaces                                (return zippedRawCodeAndLines)
--     wordsAndLines         <- mapPreserveLn (\s -> wordsPreserveStringSpacing [] s)  (return rawCodeAndLines)
--     tokensAndLines        <- mapPreserveLn (\s -> map readToken s)                  (return wordsAndLines)
--     tokenInLines2d        <- map (\(ln,ts) -> (map (\t -> TokenInLine t ln) ts))    (return tokensAndLines)
--     tokenInLines          <- concat (return tokenInLines2d :: [[TokenInLine]])
--     ignoreCommentsInLines $ consolidateNestedCollapsibleTokenInLines $ consolidateEagerCollapsibleTokenInLines $ return tokenInLines
--     where 
--         linesStrs = lines strs
--         mapPreserveLn :: (b -> c) -> [(a,b)] -> [(a,c)]
--         mapPreserveLn f xs = map (\(first,second) -> (first, f second)) xs