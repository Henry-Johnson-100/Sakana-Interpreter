{-# LANGUAGE MagicHash #-}

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
    keywordTokenIsDeclarationRequiringId,
  )
where

import qualified Control.Arrow as Bifunctor
import qualified Data.Char (isSpace)
import qualified Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Maybe (Maybe (..), fromJust, isNothing, maybe)
import qualified Exception.Base
  ( ExceptionSeverity (Fatal),
    ExceptionType
      ( IncompleteStringLiteralException,
        UndefinedTokenException
      ),
    newException,
    raiseError,
  )
import qualified Token.Bracket as B
  ( BracketTerminal (Open),
    ScopeType (Send),
    fromBracket,
    readBracket,
    repr,
  )
import qualified Token.Control as C
  ( Control (..),
    fromControl,
    readControl,
    repr,
  )
import qualified Token.Data as D
  ( Data (Comment, Id, Num, Other, String),
    fromData,
    miscRepr,
    readData,
  )
import qualified Token.Keyword as K
  ( Keyword (Fish, Migrate),
    fromKeyword,
    isDeclarationRequiringId,
    readKeyword,
    repr,
  )
import qualified Token.Operator as O (Operator (Add), fromOp, readOp, repr, spacingRepr)
import qualified Token.Util.CollapsibleTerminalCases as CTC
  ( CollapsibleTerminalCases (..),
    sameCase,
  )
import qualified Token.Util.EagerCollapsible as EagerCollapsible
  ( dropBetween,
    dropInfix,
    isEagerCollapsible,
    takeBetween,
  )
import Token.Util.Like (Like (..))
import Token.Util.String (onlyLiteral, padEqual)

data Token
  = Bracket B.ScopeType B.BracketTerminal
  | Control C.Control
  | Data D.Data
  | Keyword K.Keyword
  | Operator O.Operator
  deriving (Show, Read, Eq)

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
genericKeyword = Keyword K.Fish

genericControl :: Token
genericControl = Control C.Fin

genericOperator :: Token
genericOperator = Operator O.Add

genericBracket :: Token
genericBracket = Bracket B.Send B.Open

genericData :: Token
genericData = Data (D.Num 0)

getTokenBracketScopeType :: Token -> B.ScopeType
getTokenBracketScopeType (Bracket st _) = st

keywordTokenIsDeclarationRequiringId :: Token -> Bool
keywordTokenIsDeclarationRequiringId t =
  Data.Maybe.maybe False (K.isDeclarationRequiringId) (baseKeyword t)

dataTokenIsId :: Token -> Bool
dataTokenIsId (Data (D.Id _)) = True
dataTokenIsId _ = False

dataTokenIsOther :: Token -> Bool
dataTokenIsOther (Data (D.Other _)) = True
dataTokenIsOther _ = False

dataTokenIsString :: Token -> Bool
dataTokenIsString (Data (D.String _)) = True
dataTokenIsString _ = False

tokenPacketToUnit :: Packet Token -> [TokenUnit]
tokenPacketToUnit tp = map (\t -> PacketUnit t (packetLine tp)) (members tp)

baseData :: Token -> Data.Maybe.Maybe D.Data
baseData (Data d) = Data.Maybe.Just d
baseData _ = Data.Maybe.Nothing

baseKeyword :: Token -> Data.Maybe.Maybe K.Keyword
baseKeyword (Keyword k) = Data.Maybe.Just k
baseKeyword _ = Data.Maybe.Nothing

baseDataString :: Token -> String
baseDataString t = maybe "" D.fromData (baseData t)

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
  | isAnyReprInHeadGroup B.repr =
    padReprElemFromHeadGroup B.repr 1
      ++ addSpaces (dropReprElemFromHeadGroup B.repr str)
  | isAnyReprInHeadGroup D.miscRepr =
    padReprElemFromHeadGroup D.miscRepr 1
      ++ addSpaces (dropReprElemFromHeadGroup D.miscRepr str)
  | isAnyReprInHeadGroup O.spacingRepr =
    if length (filterReprElemsInHeadGroup O.spacingRepr) == 1
      then
        padReprElemFromHeadGroup O.spacingRepr 1
          ++ addSpaces (dropReprElemFromHeadGroup O.spacingRepr str)
      else
        padEqual
          (getLongestStringFromList (filterReprElemsInHeadGroup O.spacingRepr))
          1
          ++ addSpaces
            ( (drop . maximum)
                (map length (filterReprElemsInHeadGroup O.spacingRepr))
                str
            )
  | otherwise = head str : addSpaces (tail str)
  where
    headGroup :: String
    headGroup = take 5 str
    isAnyReprInHeadGroup :: [String] -> Bool
    isAnyReprInHeadGroup = any (`Data.List.isPrefixOf` headGroup)
    filterReprElemsInHeadGroup :: [String] -> [String]
    filterReprElemsInHeadGroup = filter (`Data.List.isPrefixOf` headGroup)
    getReprElemInHeadGroup :: [String] -> String
    getReprElemInHeadGroup reprList = head $ filterReprElemsInHeadGroup reprList
    padReprElemFromHeadGroup :: [String] -> Int -> String
    padReprElemFromHeadGroup reprList = padEqual (getReprElemInHeadGroup reprList)
    dropReprElemFromHeadGroup :: [String] -> String -> String
    dropReprElemFromHeadGroup reprList = drop (length (getReprElemInHeadGroup reprList))
    getLongestStringFromList :: [String] -> String
    getLongestStringFromList strs =
      head $ filter (\x -> length x == maximum (map length strs)) strs

tokenIsStringCollapsibleTerminalCase :: CTC.CollapsibleTerminalCases Token
tokenIsStringCollapsibleTerminalCase =
  CTC.CollapsibleTerminalCases dataTokenIsString dataTokenIsString

tokenIsStringPrefix :: Token -> Bool
tokenIsStringPrefix (Data (D.String a)) =
  ( ("\"" `Data.List.isPrefixOf` a)
      && not ("\"" `Data.List.isSuffixOf` a)
  )
    || (length a == 1)
tokenIsStringPrefix _ = False

tokenIsStringSuffix :: Token -> Bool
tokenIsStringSuffix (Data (D.String a)) =
  ( ("\"" `Data.List.isSuffixOf` a)
      && not ("\"" `Data.List.isPrefixOf` a)
  )
    || (length a == 1)
tokenIsStringSuffix _ = False

tokenIsCommentPrefix :: Token -> Bool
tokenIsCommentPrefix (Data (D.Comment a)) = "/*" `Data.List.isPrefixOf` a
tokenIsCommentPrefix _ = False

tokenIsCommentSuffix :: Token -> Bool
tokenIsCommentSuffix (Data (D.Comment a)) = "*/" `Data.List.isSuffixOf` a
tokenIsCommentSuffix _ = False

stringIsCommentPrefix :: String -> Bool
stringIsCommentPrefix a = "/*" `Data.List.isPrefixOf` a

stringIsCommentSuffix :: String -> Bool
stringIsCommentSuffix a = "*/" `Data.List.isSuffixOf` a

consolidateEagerCollapsibleTokens :: [Token] -> [Token]
consolidateEagerCollapsibleTokens [] = []
consolidateEagerCollapsibleTokens (t : ts)
  | tokenIsStringPrefix t
      && EagerCollapsible.isEagerCollapsible
        tokenIsStringCollapsibleTerminalCase
        (t : ts) =
    (Data . D.String . consolidateTokensToString) (t : ts) :
    consolidateEagerCollapsibleTokens
      (EagerCollapsible.dropBetween tokenIsStringCollapsibleTerminalCase (t : ts))
  | dataTokenIsString t =
    (Data . D.String . baseDataString) t :
    consolidateEagerCollapsibleTokens ts
  | otherwise = t : consolidateEagerCollapsibleTokens ts
  where
    consolidateTokensToString :: [Token] -> String
    consolidateTokensToString xs =
      concatMap
        baseDataString
        ( EagerCollapsible.takeBetween
            tokenIsStringCollapsibleTerminalCase
            xs
        )

wordsPreserveStringSpacing :: String -> [String]
wordsPreserveStringSpacing str = wordsPreserveStringSpacingScan [] str
  where
    wordsPreserveStringSpacingScan :: [String] -> String -> [String]
    wordsPreserveStringSpacingScan strs "" = strs
    wordsPreserveStringSpacingScan strs (s : str)
      | s == '"' =
        wordsPreserveStringSpacingScan
          (strs ++ [buildPreservedString str])
          (EagerCollapsible.dropInfix (buildPreservedString str) (s : str))
      | Data.Char.isSpace s = wordsPreserveStringSpacingScan strs str
      | otherwise =
        wordsPreserveStringSpacingScan
          (strs ++ [buildWord (s : str)])
          (EagerCollapsible.dropInfix (buildWord (s : str)) (s : str))
      where
        buildPreservedString :: String -> String
        buildPreservedString str = "\"" ++ takeWhile ('\"' /=) str ++ "\""
        buildWord :: String -> String
        buildWord = takeWhile (not . Data.Char.isSpace)

prepareRawString :: String -> [Packet String]
prepareRawString "" = []
prepareRawString strs =
  zippedLineNumbersToStringPackets $
    mapPreserveLn (wordsPreserveStringSpacing . addSpaces) $
      map incompleteStringLiteralErrorCheck## zipNumbersToLines
  where
    linesStrs = (reverse . dropWhile null . reverse . lines) strs
    zipNumbersToLines :: [(String, Int)]
    zipNumbersToLines = zip linesStrs [1 .. (length linesStrs + 1)]
    zippedLineNumbersToStringPackets :: [([String], Int)] -> [Packet String]
    zippedLineNumbersToStringPackets = map (uncurry Packet)
    mapPreserveLn :: (a -> b) -> [(a, Int)] -> [(b, Int)]
    mapPreserveLn f = map (Bifunctor.first f)
    incompleteStringLiteralErrorCheck## :: (String, Int) -> (String, Int)
    incompleteStringLiteralErrorCheck## (str, ln) =
      (incompleteStringLiteralErrorCheck# [ln] str, ln)

tokenize :: String -> [TokenUnit]
tokenize strs =
  tokenizeErrorChecking# . concatMap tokenPacketToUnit $
    ( consolidateStringTokensByLine
        . filterEmptyPackets
        . tokenizePreparedStringLines
        . prepareRawString
    )
      strs
  where
    filterEmptyPackets :: [Packet a] -> [Packet a]
    filterEmptyPackets = filter (not . null . members)
    consolidateStringTokensByLine :: [Packet Token] -> [Packet Token]
    consolidateStringTokensByLine =
      map (\ps -> Packet (consolidateEagerCollapsibleTokens (members ps)) (packetLine ps))
    tokenizePreparedStringLines :: [Packet String] -> [Packet Token]
    tokenizePreparedStringLines [] = []
    tokenizePreparedStringLines (ps : pss)
      | hasCommentPrefix ps && hasCommentSuffix ps =
        mapStringPToTokenPIgnoreSameLineComments ps : tokenizePreparedStringLines pss
      | hasCommentPrefix ps =
        mapStringPToTokenPTakeUntilComment ps :
        tokenizePreparedStringLines
          (dropWhile (not . any stringIsCommentSuffix . members) pss)
      | hasCommentSuffix ps =
        mapStringPToTokenPDropThroughComment ps : tokenizePreparedStringLines pss
      | otherwise = fmap readToken ps : tokenizePreparedStringLines pss
      where
        hasCommentPrefix :: Packet String -> Bool
        hasCommentPrefix ps' = any stringIsCommentPrefix (members ps')
        hasCommentSuffix :: Packet String -> Bool
        hasCommentSuffix ps' = any stringIsCommentSuffix (members ps')
        mapStringPToTokenPIgnoreSameLineComments :: Packet String -> Packet Token
        mapStringPToTokenPIgnoreSameLineComments ps' =
          fmap
            readToken
            ( Packet
                ( takeWhile (not . stringIsCommentPrefix) (members ps')
                    ++ tail'
                      (dropWhile (not . stringIsCommentSuffix) (members ps'))
                )
                (packetLine ps')
            )
        mapStringPToTokenPTakeUntilComment :: Packet String -> Packet Token
        mapStringPToTokenPTakeUntilComment ps' =
          fmap
            readToken
            ( Packet
                (takeWhile (not . stringIsCommentPrefix) (members ps'))
                (packetLine ps')
            )
        mapStringPToTokenPDropThroughComment :: Packet String -> Packet Token
        mapStringPToTokenPDropThroughComment ps' =
          fmap
            readToken
            ( Packet
                (tail' (dropWhile (not . stringIsCommentSuffix) (members ps')))
                (packetLine ps')
            )
        tail' :: [a] -> [a]
        tail' [] = []
        tail' (x : xs) = xs

incompleteStringLiteralErrorCheck# :: [Int] -> String -> String
incompleteStringLiteralErrorCheck# ln str
  | (odd . length . filter ('\"' ==)) str =
    Exception.Base.raiseError $
      Exception.Base.newException
        Exception.Base.IncompleteStringLiteralException
        ln
        ("Incomplete string literal: " ++ str)
        Exception.Base.Fatal
  | otherwise = str

tokenizeErrorChecking# :: [TokenUnit] -> [TokenUnit]
tokenizeErrorChecking# tus
  | (not . null . filterDataTokenIsOther) tus =
    Exception.Base.raiseError $
      Exception.Base.newException
        Exception.Base.UndefinedTokenException
        [(unitLine . head . filterDataTokenIsOther) tus]
        ("Undefined token: " ++ (fromToken . unit . head . filterDataTokenIsOther) tus)
        Exception.Base.Fatal
  | otherwise = tus
  where
    filterDataTokenIsOther :: [TokenUnit] -> [TokenUnit]
    filterDataTokenIsOther = filter (dataTokenIsOther . unit)