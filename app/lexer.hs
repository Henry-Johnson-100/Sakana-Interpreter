module Lexer
  ( Token (..),
    PacketUnit (..),
    TokenUnit (..),
    tokenize,
    fromToken,
    like,
    baseData,
    getTokenBracketScopeType,
    genericKeyword,
    genericControl,
    genericOperator,
    genericBracket,
    genericData,
    dataTokenIsId,
  )
where

import qualified Control.Arrow as Data.Bifunctor
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Tuple (uncurry)
import Exception.Base
import Token.Bracket as B
  ( BracketTerminal (Open),
    ScopeType (Send),
    fromBracket,
    readBracket,
    repr,
  )
import Token.Control as C
  ( Control (..),
    fromControl,
    readControl,
    repr,
  )
import Token.Data as D
  ( Data (..),
    fromData,
    miscRepr,
    readData,
  )
import Token.Keyword as K
  ( Keyword (Fish),
    fromKeyword,
    readKeyword,
    repr,
  )
import Token.Operator as O (Operator (Add), fromOp, readOp, repr, spacingRepr)
import Token.Util.EagerCollapsible
  ( dropBetween,
    dropInfix,
    isEagerCollapsible,
    takeBetween,
  )
import Token.Util.Like (Like (..))
import Token.Util.String (padEqual)

data Token = Bracket ScopeType BracketTerminal | Control Control | Data Data | Keyword Keyword | Operator Operator deriving (Show, Read, Eq)

data PacketUnit a = PacketUnit
  { unit :: a,
    unitLine :: Int
  }
  deriving (Show, Read, Eq)

type TokenUnit = PacketUnit Token

data Packet a = Packet
  { members :: [a],
    packetLine :: Int
  }
  deriving (Show, Eq)

instance Like Token where
  like (Bracket _ _) (Bracket _ _) = True
  like (Control _) (Control _) = True
  like (Data _) (Data _) = True
  like (Keyword _) (Keyword _) = True
  like (Operator _) (Operator _) = True
  like _ _ = False
  notLike a b = not $ like a b

instance Functor Packet where
  fmap f sp = Packet (fmap f (members sp)) (packetLine sp)

genericKeyword :: Token
genericKeyword = Keyword Fish

genericControl :: Token
genericControl = Control Fin

genericOperator :: Token
genericOperator = Operator Add

genericBracket :: Token
genericBracket = Bracket Send Open

genericData :: Token
genericData = Data (Int 0)

getTokenBracketScopeType :: Token -> ScopeType
getTokenBracketScopeType (Bracket st _) = st

dataTokenIsId :: Token -> Bool
dataTokenIsId (Data (Id _)) = True
dataTokenIsId _ = False

dataTokenIsOther :: Token -> Bool
dataTokenIsOther (Data (Other _)) = True
dataTokenIsOther _ = False

tokenPacketToUnit :: Packet Token -> [TokenUnit]
tokenPacketToUnit tp = map (\t -> PacketUnit t (packetLine tp)) (members tp)

baseData :: Token -> Data
baseData (Data d) = d

baseDataString :: Token -> String
baseDataString t = D.fromData $ baseData t

fromToken :: Token -> String
fromToken (Bracket st bt) = B.fromBracket st bt
fromToken (Lexer.Control control) = C.fromControl control
fromToken (Data d) = D.fromData d
fromToken (Keyword keyword) = K.fromKeyword keyword
fromToken (Operator operator) = O.fromOp operator

fromTokenUnit :: TokenUnit -> String
fromTokenUnit tu = fromToken $ unit tu

readToken :: String -> Token
readToken str
  | str `elem` K.repr = Keyword (K.readKeyword str)
  | str `elem` B.repr = uncurry Bracket (B.readBracket str)
  | str `elem` C.repr = Lexer.Control (C.readControl str)
  | str `elem` O.repr = Operator (O.readOp str)
  | otherwise = Data (D.readData str)

addSpaces :: String -> String
addSpaces str
  | null str = ""
  | isAnyReprInHeadGroup B.repr = padReprElemFromHeadGroup B.repr 1 ++ addSpaces (dropReprElemFromHeadGroup B.repr str)
  | isAnyReprInHeadGroup D.miscRepr = padReprElemFromHeadGroup D.miscRepr 1 ++ addSpaces (dropReprElemFromHeadGroup D.miscRepr str)
  | isAnyReprInHeadGroup O.spacingRepr =
    if length (filterReprElemsInHeadGroup O.spacingRepr) == 1
      then padReprElemFromHeadGroup O.spacingRepr 1 ++ addSpaces (dropReprElemFromHeadGroup O.spacingRepr str)
      else padEqual (getLongestStringFromList (filterReprElemsInHeadGroup O.spacingRepr)) 1 ++ addSpaces (drop (maximum (map length (filterReprElemsInHeadGroup O.spacingRepr))) str)
  | otherwise = head str : addSpaces (tail str)
  where
    headGroup :: String
    headGroup = take 5 str
    isAnyReprInHeadGroup :: [String] -> Bool
    isAnyReprInHeadGroup = any (`isPrefixOf` headGroup)
    filterReprElemsInHeadGroup :: [String] -> [String]
    filterReprElemsInHeadGroup = filter (`isPrefixOf` headGroup)
    getReprElemInHeadGroup :: [String] -> String
    getReprElemInHeadGroup reprList = head $ filterReprElemsInHeadGroup reprList
    padReprElemFromHeadGroup :: [String] -> Int -> String
    padReprElemFromHeadGroup reprList = padEqual (getReprElemInHeadGroup reprList)
    dropReprElemFromHeadGroup :: [String] -> String -> String
    dropReprElemFromHeadGroup reprList = drop (length (getReprElemInHeadGroup reprList))
    getLongestStringFromList :: [String] -> String
    getLongestStringFromList strs = head $ filter (\x -> length x == maximum (map length strs)) strs

tokenIsStringPrefix :: Token -> Bool
tokenIsStringPrefix (Data (D.String a)) = (("\"" `isPrefixOf` a) && not ("\"" `isSuffixOf` a)) || (length a == 1)
tokenIsStringPrefix _ = False

tokenIsStringSuffix :: Token -> Bool
tokenIsStringSuffix (Data (D.String a)) = (("\"" `isSuffixOf` a) && not ("\"" `isPrefixOf` a)) || (length a == 1)
tokenIsStringSuffix _ = False

tokenIsCommentPrefix :: Token -> Bool
tokenIsCommentPrefix (Data (D.Comment a)) = "/*" `isPrefixOf` a
tokenIsCommentPrefix _ = False

tokenIsCommentSuffix :: Token -> Bool
tokenIsCommentSuffix (Data (D.Comment a)) = "*/" `isSuffixOf` a
tokenIsCommentSuffix _ = False

stringIsCommentPrefix :: String -> Bool
stringIsCommentPrefix a = "/*" `isPrefixOf` a

stringIsCommentSuffix :: String -> Bool
stringIsCommentSuffix a = "*/" `isSuffixOf` a

consolidateEagerCollapsibleTokens :: [Token] -> [Token]
consolidateEagerCollapsibleTokens [] = []
consolidateEagerCollapsibleTokens (t : ts)
  | tokenIsStringPrefix t && isEagerCollapsible tokenIsStringPrefix tokenIsStringSuffix (t : ts) = mapToConsolidatedData (Data (D.String "")) (t : ts) ++ consolidateEagerCollapsibleTokens (dropBetween tokenIsStringPrefix tokenIsStringSuffix (t : ts))
  | otherwise = t : consolidateEagerCollapsibleTokens ts
  where
    mapTakeBetween :: Token -> [Token] -> [Token]
    mapTakeBetween emptyTokenDataType xs = map (constructDataToken emptyTokenDataType . D.fromData . baseData) $ takeBetween (isDataTypePrefix emptyTokenDataType) (isDataTypeSuffix emptyTokenDataType) xs
    mapToConsolidatedData :: Token -> [Token] -> [Token]
    mapToConsolidatedData emptyTokenDataType xs = [constructDataToken emptyTokenDataType (concatMap (D.fromData . baseData) (mapTakeBetween emptyTokenDataType xs))]
    isDataTypePrefix :: Token -> Token -> Bool
    isDataTypePrefix (Data (D.String _)) = tokenIsStringPrefix
    isDataTypeSuffix :: Token -> Token -> Bool
    isDataTypeSuffix (Data (D.String _)) = tokenIsStringSuffix
    constructDataToken :: Token -> String -> Token
    constructDataToken (Data (D.String _)) str = Data (D.String str)

wordsPreserveStringSpacing :: String -> [String]
wordsPreserveStringSpacing str = wordsPreserveStringSpacingScan [] str
  where
    wordsPreserveStringSpacingScan :: [String] -> String -> [String]
    wordsPreserveStringSpacingScan strs "" = strs
    wordsPreserveStringSpacingScan strs (s : str)
      | s == '"' = wordsPreserveStringSpacingScan (strs ++ [buildPreservedString str]) (dropInfix (buildPreservedString str) (s : str))
      | isSpace s = wordsPreserveStringSpacingScan strs str
      | otherwise = wordsPreserveStringSpacingScan (strs ++ [buildWord (s : str)]) (dropInfix (buildWord (s : str)) (s : str))
      where
        buildPreservedString :: String -> String
        buildPreservedString str = "\"" ++ takeWhile ('\"' /=) str ++ "\""
        buildWord :: String -> String
        buildWord = takeWhile (not . isSpace)

prepareRawString :: String -> [Packet String]
prepareRawString "" = []
prepareRawString strs = zippedLineNumbersToStringPackets $ mapPreserveLn (wordsPreserveStringSpacing . addSpaces) $ map incompleteStringLiteralErrorCheck' zipNumbersToLines
  where
    linesStrs = (reverse . dropWhile null . reverse . lines) strs
    zipNumbersToLines :: [(String, Int)]
    zipNumbersToLines = zip linesStrs [1 .. (length linesStrs + 1)]
    zippedLineNumbersToStringPackets :: [([String], Int)] -> [Packet String]
    zippedLineNumbersToStringPackets = map (uncurry Packet)
    mapPreserveLn :: (a -> b) -> [(a, Int)] -> [(b, Int)]
    mapPreserveLn f = map (Data.Bifunctor.first f)
    incompleteStringLiteralErrorCheck' :: (String, Int) -> (String, Int)
    incompleteStringLiteralErrorCheck' (str, ln) = (incompleteStringLiteralErrorCheck [ln] str, ln)

tokenize :: String -> [TokenUnit]
tokenize strs = tokenizeErrorChecking . concatMap tokenPacketToUnit $ (consolidateStringTokensByLine . filterEmptyPackets . tokenizePreparedStringLines . prepareRawString) strs
  where
    filterEmptyPackets :: [Packet a] -> [Packet a]
    filterEmptyPackets = filter (not . null . members)
    consolidateStringTokensByLine :: [Packet Token] -> [Packet Token]
    consolidateStringTokensByLine = map (\ps -> Packet (consolidateEagerCollapsibleTokens (members ps)) (packetLine ps))
    tokenizePreparedStringLines :: [Packet String] -> [Packet Token]
    tokenizePreparedStringLines [] = []
    tokenizePreparedStringLines (ps : pss)
      | hasCommentPrefix ps && hasCommentSuffix ps = mapStringPToTokenPIgnoreSameLineComments ps : tokenizePreparedStringLines pss
      | hasCommentPrefix ps = mapStringPToTokenPTakeUntilComment ps : tokenizePreparedStringLines (dropWhile (not . any stringIsCommentSuffix . members) pss)
      | hasCommentSuffix ps = mapStringPToTokenPDropThroughComment ps : tokenizePreparedStringLines pss
      | otherwise = fmap readToken ps : tokenizePreparedStringLines pss
      where
        hasCommentPrefix :: Packet String -> Bool
        hasCommentPrefix ps' = any stringIsCommentPrefix (members ps')
        hasCommentSuffix :: Packet String -> Bool
        hasCommentSuffix ps' = any stringIsCommentSuffix (members ps')
        mapStringPToTokenPIgnoreSameLineComments :: Packet String -> Packet Token
        mapStringPToTokenPIgnoreSameLineComments ps' = fmap readToken (Packet (takeWhile (not . stringIsCommentPrefix) (members ps') ++ tail' (dropWhile (not . stringIsCommentSuffix) (members ps'))) (packetLine ps'))
        mapStringPToTokenPTakeUntilComment :: Packet String -> Packet Token
        mapStringPToTokenPTakeUntilComment ps' = fmap readToken (Packet (takeWhile (not . stringIsCommentPrefix) (members ps')) (packetLine ps'))
        mapStringPToTokenPDropThroughComment :: Packet String -> Packet Token
        mapStringPToTokenPDropThroughComment ps' = fmap readToken (Packet (tail' (dropWhile (not . stringIsCommentSuffix) (members ps'))) (packetLine ps'))
        tail' :: [a] -> [a]
        tail' [] = []
        tail' (x : xs) = xs

incompleteStringLiteralErrorCheck :: [Int] -> String -> String
incompleteStringLiteralErrorCheck ln str
  | (odd . length . filter ('\"' ==)) str = raiseError $ newException IncompleteStringLiteralException ln ("Incomplete string literal: " ++ str) Fatal
  | otherwise = str

tokenizeErrorChecking :: [TokenUnit] -> [TokenUnit]
tokenizeErrorChecking tus
  | (not . null . filterDataTokenIsOther) tus = raiseError $ newException UndefinedTokenException  [(unitLine . head . filterDataTokenIsOther) tus] ("Undefined token: " ++ (fromToken . unit . head . filterDataTokenIsOther) tus) Fatal
  | otherwise = tus
  where
    filterDataTokenIsOther :: [TokenUnit] -> [TokenUnit]
    filterDataTokenIsOther = filter (dataTokenIsOther . unit)