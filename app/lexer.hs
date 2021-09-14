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


consolidateNestedCollapsibleTokens :: [Token] -> [Token]
consolidateNestedCollapsibleTokens [] = []
consolidateNestedCollapsibleTokens ts 
    | hasNestedCollapsible isCommentPrefix isCommentSuffix ts = consolidateNestedCollapsibleTokens $ foldr (++) [] $ unwrapPartition $ partitionNests isCommentPrefix isCommentSuffix ts
    | otherwise                                               = ts


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


tokenize :: String -> [Token]
tokenize strs = ignoreComments $ consolidateNestedCollapsibleTokens $ consolidateEagerCollapsibleTokens $ map (readToken) $ wordsPreserveStringSpacing [] $ addSpaces strs