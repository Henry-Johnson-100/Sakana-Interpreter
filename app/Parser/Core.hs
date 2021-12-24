module Parser.Core
  ( parse',
    getParseError,
    boolParser,
    stringParser,
    numParser,
    idParser,
    dataParser,
    genericKeywordParser,
    genericBracketParser,
    dataTokenParser,
    dataIdParser,
    keywordTokenParser,
    inBracketParser,
    keywordTreeParser,
    idTreeParser,
    dataTreeParser,
    lampreyParser,
    functionDefinitionParser,
    functionCallParser,
    swimParser,
    shoalParser,
    expressionParser,
    globalStatementParser,
    lampreyParameterParser,
    docParser,
    generalParse,
  )
where

import qualified Control.Monad as CMonad
import qualified Data.Char as DChar
import qualified Data.Either as DEither
import qualified Data.Functor.Identity as DFId
import qualified Data.List as DList
import qualified Data.Maybe as DMaybe
{-
For Text.Parsec
Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.
-}

import qualified Exception.Base as Exception
import qualified Syntax
import Text.Parsec ((<?>), (<|>))
import qualified Text.Parsec as Prs
import qualified Text.Parsec.Error
import qualified Util.Classes as UC
import Util.General ((.<))
import qualified Util.General as UGen
import Util.Tree ((-<-), (-<=))
import qualified Util.Tree as Tree

type KeywordParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.Keyword

type DataParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.Data

type TokenParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.Token

type TokenSourceParser u = Prs.ParsecT [Char] u DFId.Identity Syntax.TokenSource

type TreeParser u = Prs.ParsecT [Char] u DFId.Identity [Syntax.SyntaxTree]

----Data Parsers--------------------------------------------------------------------------
------------------------------------------------------------------------------------------

boolParser :: DataParser u
boolParser = do
  b <- Prs.string "True" <|> Prs.string "False"
  (return . Syntax.Boolean . readBool) b
  where
    readBool :: String -> Bool
    readBool = read

stringParser :: DataParser u
stringParser = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '"'
  string <-
    Prs.manyTill
      ( (Prs.choice . (<$>) Prs.try)
          [removeInvisibleSpacing, unescapeEscapedSpaces, anyCharAsString]
      )
      (Prs.char '"')
  (return . Syntax.String . concat) string
  where
    anyCharAsString :: Prs.ParsecT [Char] u DFId.Identity [Char]
    anyCharAsString = do
      ch <- Prs.anyChar
      return [ch]
    removeInvisibleSpacing :: Prs.ParsecT [Char] u DFId.Identity [Char]
    removeInvisibleSpacing = do
      (Prs.choice . (<$>) Prs.try) [Prs.tab, Prs.newline, Prs.crlf]
      return ""
    unescapeEscapedSpaces :: Prs.ParsecT [Char] u DFId.Identity [Char]
    unescapeEscapedSpaces =
      (Prs.choice . (<$>) Prs.try)
        [unescapeEscapedNewline, unescapeEscapedTab, unescapeEscapedCarriageReturn]
      where
        unescapeEscapedNewline :: Prs.ParsecT [Char] u DFId.Identity [Char]
        unescapeEscapedNewline = do
          Prs.string "\\n"
          return "\n"
        unescapeEscapedTab :: Prs.ParsecT [Char] u DFId.Identity [Char]
        unescapeEscapedTab = do
          Prs.string "\\t"
          return "\t"
        unescapeEscapedCarriageReturn :: Prs.ParsecT [Char] u DFId.Identity [Char]
        unescapeEscapedCarriageReturn = do
          Prs.string "\\r"
          return "\r"

numParser :: DataParser u
numParser = do
  maybeNegation <- (Prs.optionMaybe . Prs.char) '-'
  integerDigits <- Prs.many1 Prs.digit
  maybeDecimal <- (Prs.optionMaybe . Prs.char) '.'
  decimalDigits <-
    if DMaybe.isJust maybeDecimal then (Prs.many1 Prs.digit) else Prs.parserZero
  -- maybeDecimalDigits <- (Prs.optionMaybe . Prs.many1) Prs.digit
  (return . Syntax.Num . readDouble) $
    composeNum maybeNegation integerDigits maybeDecimal decimalDigits
  where
    readDouble :: String -> Double
    readDouble = read
    composeNum :: Maybe Char -> [Char] -> Maybe Char -> [Char] -> [Char]
    composeNum mNeg intD mDec decD =
      let neg = DMaybe.maybe [] UGen.listSingleton mNeg
          dec = DMaybe.maybe [] UGen.listSingleton mDec
       in neg ++ intD ++ dec ++ decD

-- | A special kind of DataParser, since ID's are not evaluable data types, this parser
-- is NOT included in the
--
-- > dataParser :: DataParser u
--
-- function.
idParser :: DataParser u
idParser = do
  accessorPrefixes <- (Prs.many . Prs.try) accessorIdParser
  baseName <- Prs.many1 validIdCharParser
  (return . Syntax.Id . (++) (concat accessorPrefixes)) baseName
  where
    validIdCharParser :: Prs.ParsecT [Char] u DFId.Identity Char
    validIdCharParser =
      (Prs.choice . (<$>) Prs.try)
        [ Prs.letter,
          Prs.oneOf "!@#$%^&*-=_+,<>/?;:|`~[]{}"
        ]
        <?> "valid ID character: alphanumeric character or symbolic character, \
            \excluding: \' . \' \" \\  \'"
    accessorIdParser :: Prs.ParsecT [Char] u DFId.Identity [Char]
    accessorIdParser = do
      accessorId <- Prs.many1 validIdCharParser
      dot <- Prs.char '.'
      (return . (++) accessorId) [dot]

dataParser :: DataParser u
dataParser =
  (Prs.choice . (<$>) Prs.try) [numParser, stringParser, boolParser]

----Keyword Parsers-----------------------------------------------------------------------
------------------------------------------------------------------------------------------

genericKeywordParser :: Syntax.Keyword -> KeywordParser u
genericKeywordParser k = do
  kStr <- (Prs.string . map DChar.toLower . show) k
  return k

----Token Parsers-------------------------------------------------------------------------
------------------------------------------------------------------------------------------

genericBracketParser :: Syntax.ScopeType -> Syntax.BracketTerminal -> TokenParser u
genericBracketParser st bt = do
  stChar <- (Prs.char . Syntax.fromScopeType) st
  btChar <- (Prs.char . Syntax.fromTerminal) bt
  (return .< Syntax.Bracket) st bt

dataTokenParser :: TokenParser u
dataTokenParser = do
  d <- dataParser
  (return . Syntax.Data) d

dataIdParser :: TokenParser u
dataIdParser = do
  identification <- idParser
  (return . Syntax.Data) identification

keywordTokenParser :: Syntax.Keyword -> TokenParser u
keywordTokenParser = (=<<) (return . Syntax.Keyword) . genericKeywordParser

----TokenSource Parsers-------------------------------------------------------------------
------------------------------------------------------------------------------------------

tokenInfoParser :: TokenParser u -> TokenSourceParser u
tokenInfoParser tp = do
  pos <- Prs.getPosition
  t <- tp
  let ln = Prs.sourceLine pos
  (return .< Syntax.Source) t ln

----Combinators and Util functions--------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Takes a parser and ignores the spaces before and after it.
stripSpaces ::
  Prs.ParsecT [Char] u DFId.Identity a -> Prs.ParsecT [Char] u DFId.Identity a
stripSpaces p = do
  Prs.spaces
  p
  Prs.spaces
  p

attachAllBranches :: Tree.Tree a -> [[Tree.Tree a]] -> Tree.Tree a
attachAllBranches h trs = DList.foldl' (-<=) h trs

infixl 9 -<*=

-- | Equal to attachAllBranches.
--
-- Strongly left associative.
--
-- > a -<*= b -<= c -<*= d
--
-- Is the same as
--
-- > ((a -<*= b) -<= c) -<*= d
(-<*=) :: Tree.Tree a -> [[Tree.Tree a]] -> Tree.Tree a
(-<*=) = attachAllBranches

inBracketParser :: Syntax.ScopeType -> TreeParser u -> TreeParser u
inBracketParser st tp = do
  (genericBracketParser st) Syntax.Open
  Prs.spaces
  tp
  Prs.spaces
  (genericBracketParser st) Syntax.Close
  tp

----Tree Parsers--------------------------------------------------------------------------
------------------------------------------------------------------------------------------

keywordTreeParser :: Syntax.Keyword -> Syntax.ScopeType -> TreeParser u
keywordTreeParser k st = do
  keywordSource <- (tokenInfoParser . keywordTokenParser) k
  let keywordTree = (Tree.tree . Syntax.sourceToSyntaxUnit keywordSource) st
  return [keywordTree]

idTreeParser :: Syntax.ScopeType -> TreeParser u
idTreeParser st = do
  identification <- (tokenInfoParser) dataIdParser
  (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit identification) st

dataTreeParser :: Syntax.ScopeType -> TreeParser u
dataTreeParser st = do
  d <- (tokenInfoParser) dataTokenParser
  (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit d) st

-- #TODO
nullBracketParser :: Syntax.ScopeType -> TreeParser u
nullBracketParser st = do
  (genericBracketParser st) Syntax.Open
  Prs.spaces
  (genericBracketParser st) Syntax.Close
  return [UC.empty]

lampreyParser :: Syntax.ScopeType -> TreeParser u
lampreyParser st = do
  implicitKeyword <-
    (Prs.optionMaybe . tokenInfoParser . keywordTokenParser) Syntax.Lamprey
  paramsOrOther <-
    ( Prs.many
        . inBracketParser Syntax.Send
        . lampreyParameterParser
      )
      Syntax.Send
  value <- (inBracketParser Syntax.Return . expressionParser) Syntax.Return
  let lampreyLn = (Syntax.line . DMaybe.fromJust . (=<<) Tree.treeNode . UGen.head') value
      genericLamprey =
        Syntax.SyntaxUnit (Syntax.Keyword Syntax.Lamprey) lampreyLn st
      justLampreyTree =
        ( Tree.tree
            . DMaybe.maybe
              (genericLamprey)
              (flip Syntax.sourceToSyntaxUnit st)
        )
          implicitKeyword
      lampreyTree =
        justLampreyTree -<*= paramsOrOther -<= value
  return [lampreyTree]

functionDefinitionParser :: Syntax.ScopeType -> TreeParser u
functionDefinitionParser st = do
  fish <- (tokenInfoParser . keywordTokenParser) Syntax.Fish
  functionId <- (idTreeParser) st
  assocLamprey <- (lampreyParser) Syntax.Return
  let fishTR = (Tree.tree . Syntax.sourceToSyntaxUnit fish) st
      idTR = (DMaybe.fromJust . UGen.head') functionId
      fishTree = fishTR -<- (idTR -<= assocLamprey)
  return [fishTree]

functionCallParser :: Syntax.ScopeType -> TreeParser u
functionCallParser st = do
  functionCallId <- (idTreeParser) st
  arguments <-
    (Prs.many . inBracketParser Syntax.Send . expressionParser) st
  let functionCallTree = ((DMaybe.fromJust . UGen.head') functionCallId) -<*= arguments
  return [functionCallTree]

swimParser :: Syntax.ScopeType -> TreeParser u
swimParser st = do
  swimKeyword <- (keywordTreeParser Syntax.Swim) st
  inSendContext <-
    (Prs.many . inBracketParser Syntax.Send) eitherFishBindOrExpr
  returnValue <-
    (inBracketParser Syntax.Return . expressionParser) Syntax.Return
  let swimTreeHead = head swimKeyword
      swimTree = swimTreeHead -<*= inSendContext -<= returnValue
  return [swimTree]
  where
    eitherFishBindOrExpr :: TreeParser u
    eitherFishBindOrExpr =
      (Prs.choice . (<$>) Prs.try) [expressionParser Syntax.Send, fishBindParser]
      where
        fishBindParser :: TreeParser u
        fishBindParser = do
          bindId <- (idTreeParser) Syntax.Send
          bindingExpression <-
            (inBracketParser Syntax.Return . expressionParser) Syntax.Return
          let bindIdTr = head bindId
              fishBindTree = bindIdTr -<= bindingExpression
          return [fishBindTree]

shoalParser :: Syntax.ScopeType -> TreeParser u
shoalParser st = do
  shoalKeyword <- (keywordTreeParser Syntax.Shoal) st
  shoalMembers <-
    (Prs.many . inBracketParser Syntax.Send . idTreeParser) Syntax.Send
  let shoalTree = (head shoalKeyword) -<*= shoalMembers
  return [shoalTree]

expressionParser :: Syntax.ScopeType -> TreeParser u
expressionParser st =
  (Prs.choice . (<$>) Prs.try)
    [ lampreyParser st,
      dataTreeParser st,
      functionCallParser st,
      swimParser st
    ]

globalStatementParser :: TreeParser u
globalStatementParser =
  (Prs.choice . (<$>) Prs.try)
    [functionDefinitionParser Syntax.Return, shoalParser Syntax.Return]

lampreyParameterParser :: Syntax.ScopeType -> TreeParser u
lampreyParameterParser st =
  Prs.choice . (<$>) Prs.try $
    [ functionDefinitionParser st,
      idTreeParser st
    ]

----Parse---------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

docParser :: Prs.ParsecT [Char] u DFId.Identity Syntax.SyntaxTree
docParser = do
  globalStatements <- Prs.many globalStatementParser
  let mainTree = Tree.tree (UC.empty {Syntax.token = Syntax.Data (Syntax.Id "main")})
      docTree = mainTree -<*= globalStatements
  return docTree

generalParse ::
  Eq a => Prs.ParsecT [Char] () DFId.Identity a -> Prs.SourceName -> String -> a
generalParse p srcName src =
  DEither.either (Exception.raiseError . getParseError) (id) (Prs.parse p srcName src)

-- parse :: Prs.SourceName -> [Char] -> Syntax.SyntaxTree
-- parse =
--   DEither.either
--     (Exception.raiseError . getParseError)
--     id
--     .< parse'

getParseError :: Prs.ParseError -> Exception.Exception
getParseError prsErr =
  let errLine = (Prs.sourceLine . Prs.errorPos) prsErr
      errMsg = '\n' : show prsErr
   in Exception.newException Exception.FailedToParse [errLine] errMsg Exception.Fatal

parse' :: Prs.SourceName -> [Char] -> Either Prs.ParseError Syntax.SyntaxTree
parse' srcName src = Prs.parse (docParser) srcName src