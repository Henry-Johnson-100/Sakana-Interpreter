module SakanaParser
  (
  )
where

import qualified Control.Monad as CMonad
import qualified Data.Char as DChar
import qualified Data.Either as DEither
import qualified Data.Functor.Identity as DFId
import qualified Data.List as DList
import qualified Data.Maybe as DMaybe
import qualified Syntax
{-
For Text.Parsec
Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.
-}
import Text.Parsec ((<?>), (<|>))
import qualified Text.Parsec as Prs
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
        [ Prs.alphaNum,
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

keywordParser :: Prs.ParsecT [Char] u DFId.Identity Syntax.Keyword
keywordParser = do
  keywordString <-
    Prs.choice (Prs.try <$> (Prs.string <$> Syntax.keywords))
      <?> ("keyword from: " ++ DList.intercalate ", " Syntax.keywords)
  (return . readKeyword) keywordString
  where
    readKeyword :: String -> Syntax.Keyword
    readKeyword =
      read . CMonad.liftM2 (:) (DChar.toUpper . head) (tail)

-- I should really learn template Haskell just for these...

genericKeywordParser :: Syntax.Keyword -> KeywordParser u
genericKeywordParser k = do
  kStr <- (Prs.string . map DChar.toLower . show) k
  return k

fishKeywordParser :: KeywordParser u
fishKeywordParser = genericKeywordParser Syntax.Fish

schoolKeywordParser :: KeywordParser u
schoolKeywordParser = genericKeywordParser Syntax.School

shoalKeywordParser :: KeywordParser u
shoalKeywordParser = genericKeywordParser Syntax.Shoal

swimKeywordParser :: KeywordParser u
swimKeywordParser = genericKeywordParser Syntax.Swim

lampreyKeywordParser :: KeywordParser u
lampreyKeywordParser = genericKeywordParser Syntax.Lamprey

----Token Parsers-------------------------------------------------------------------------
------------------------------------------------------------------------------------------

genericBracketParser :: Syntax.ScopeType -> Syntax.BracketTerminal -> TokenParser u
genericBracketParser st bt = do
  stChar <- (Prs.char . Syntax.fromScopeType) st
  btChar <- (Prs.char . Syntax.fromTerminal) bt
  (return .< Syntax.Bracket) st bt

sendOpenParser :: TokenParser u
sendOpenParser = genericBracketParser Syntax.Send Syntax.Open

sendCloseParser :: TokenParser u
sendCloseParser = genericBracketParser Syntax.Send Syntax.Close

returnOpenParser :: TokenParser u
returnOpenParser = genericBracketParser Syntax.Return Syntax.Open

returnCloseParser :: TokenParser u
returnCloseParser = genericBracketParser Syntax.Return Syntax.Close

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
attachAllBranches h [] = h
attachAllBranches h (c : cs) = (h -<= c) `attachAllBranches` cs

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
  stripSpaces $ if st == Syntax.Send then sendOpenParser else returnOpenParser
  stripSpaces tp
  stripSpaces $ if st == Syntax.Send then sendCloseParser else returnCloseParser
  tp

----Tree Parsers--------------------------------------------------------------------------
------------------------------------------------------------------------------------------

keywordTreeParser :: Syntax.Keyword -> Syntax.ScopeType -> TreeParser u
keywordTreeParser k st = do
  keywordSource <- (stripSpaces . tokenInfoParser . keywordTokenParser) k
  let keywordTree = (Tree.tree . Syntax.sourceToSyntaxUnit keywordSource) st
  return [keywordTree]

idTreeParser :: Syntax.ScopeType -> TreeParser u
idTreeParser st = do
  identification <- (stripSpaces . tokenInfoParser) dataIdParser
  (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit identification) st

dataTreeParser :: Syntax.ScopeType -> TreeParser u
dataTreeParser st = do
  d <- (stripSpaces . tokenInfoParser) dataTokenParser
  (return . UGen.listSingleton . Tree.tree . Syntax.sourceToSyntaxUnit d) st

nullBracketParser :: Syntax.ScopeType -> TreeParser u
nullBracketParser st = do
  stripSpaces $ if st == Syntax.Send then sendOpenParser else returnOpenParser
  stripSpaces $ if st == Syntax.Send then sendCloseParser else returnCloseParser
  return [UC.empty]

lampreyParser :: Syntax.ScopeType -> TreeParser u
lampreyParser st = do
  implicitKeyword <-
    (stripSpaces . Prs.optionMaybe . tokenInfoParser . keywordTokenParser) Syntax.Lamprey
  paramsOrOther <-
    (stripSpaces . Prs.many . inBracketParser Syntax.Send . statementParser) Syntax.Send
  value <- (stripSpaces . inBracketParser Syntax.Return . expressionParser) Syntax.Return
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
  fish <- (stripSpaces . tokenInfoParser . keywordTokenParser) Syntax.Fish
  functionId <- (stripSpaces . idTreeParser) st
  assocLamprey <- (stripSpaces . lampreyParser) Syntax.Return
  let fishTR = (Tree.tree . Syntax.sourceToSyntaxUnit fish) st
      idTR = (DMaybe.fromJust . UGen.head') functionId
      fishTree = fishTR -<- (idTR -<= assocLamprey)
  return [fishTree]

functionCallParser :: Syntax.ScopeType -> TreeParser u
functionCallParser st = do
  functionCallId <- (stripSpaces . idTreeParser) st
  arguments <-
    (stripSpaces . Prs.many . inBracketParser Syntax.Send . expressionParser) st
  let functionCallTree = ((DMaybe.fromJust . UGen.head') functionCallId) -<*= arguments
  return [functionCallTree]

swimParser :: Syntax.ScopeType -> TreeParser u
swimParser st = do
  swimKeyword <- (stripSpaces . keywordTreeParser Syntax.Swim) st
  inSendContext <-
    (stripSpaces . Prs.many . inBracketParser Syntax.Send) eitherFishBindOrExpr
  returnValue <-
    (stripSpaces . inBracketParser Syntax.Return . expressionParser) Syntax.Return
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
          bindId <- (stripSpaces . idTreeParser) Syntax.Send
          bindingExpression <-
            (stripSpaces . inBracketParser Syntax.Return . expressionParser) Syntax.Return
          let bindIdTr = head bindId
              fishBindTree = bindIdTr -<= bindingExpression
          return [fishBindTree]

expressionParser :: Syntax.ScopeType -> TreeParser u
expressionParser st =
  (Prs.choice . (<$>) Prs.try)
    [ lampreyParser st,
      dataTreeParser st,
      functionCallParser st,
      swimParser st
    ]

statementParser :: Syntax.ScopeType -> TreeParser u
statementParser st =
  Prs.choice . (<$>) Prs.try $
    [ functionDefinitionParser st,
      idTreeParser st
    ]

globalStatementParser :: TreeParser u
globalStatementParser = do
  return [UC.empty]

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- data PacketUnit a = PacketUnit
--   { unit :: a,
--     unitLine :: Int
--   }
--   deriving (Show, Read, Eq)

-- data SyntaxUnit = SyntaxUnit
--   { token :: Token,
--     line :: Int,
--     context :: B.ScopeType
--   }
--   deriving (Show, Eq)

-- type TokenUnit = PacketUnit Token

-- type SyntaxTree = Tree.Tree SyntaxUnit

-- type SakanaTokenParser u = Prs.ParsecT [Char] u DFId.Identity TokenUnit

-- type SakanaTreeParser u = Prs.ParsecT [Char] u DFId.Identity [SyntaxTree]

-- genericSyntaxUnit :: Token -> SyntaxUnit
-- genericSyntaxUnit t = SyntaxUnit t 0 B.Return

-- getSyntaxAttributeFromTree :: (SyntaxUnit -> b) -> SyntaxTree -> b
-- getSyntaxAttributeFromTree attr =
--   Tree.maybeOnTreeNode ((attr . genericSyntaxUnit . Data) D.Null) (attr)

-- nthChildMeetsCondition :: Int -> (SyntaxTree -> Bool) -> SyntaxTree -> Bool
-- nthChildMeetsCondition n f st
--   | n < 0 = nthChildMeetsCondition ((length . Tree.treeChildren) st + n) f st
--   | n > ((length . Tree.treeChildren) st - 1) = False
--   | otherwise = (f . (!! n) . Tree.treeChildren) st

-- setContext :: B.ScopeType -> SyntaxUnit -> SyntaxUnit
-- setContext st su = su {context = st}

-- tokenUnitToSyntaxUnit :: TokenUnit -> B.ScopeType -> SyntaxUnit
-- tokenUnitToSyntaxUnit tu = SyntaxUnit (unit tu) (unitLine tu)

-- dataDecimal :: Prs.ParsecT [Char] u DFId.Identity [Char]
-- dataDecimal = do
--   dot <- Prs.char '.'
--   decimal <- Prs.many1 Prs.digit
--   return (dot : decimal)

-- dataDouble :: SakanaTokenParser u
-- dataDouble = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   negative <- Prs.optionMaybe (Prs.char '-')
--   num <- (Prs.many1 Prs.digit)
--   maybeDecimal <- Prs.optionMaybe dataDecimal
--   let justNumStr =
--         (DMaybe.maybe [] (\x -> id x : []) negative)
--           ++ (DMaybe.maybe (num) (num ++) maybeDecimal)
--   (return . flip PacketUnit ln . Data . D.readData) justNumStr

-- dataBoolean :: SakanaTokenParser u
-- dataBoolean = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   b <- Prs.string "True" <|> Prs.string "False"
--   (return . flip PacketUnit ln . Data . D.readData) b

-- dataNull :: SakanaTokenParser u
-- dataNull = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   (return . flip PacketUnit ln . Data) D.Null

-- dataData :: SakanaTokenParser u
-- dataData = Prs.choice (Prs.try <$> [dataDouble, stringParser, dataBoolean])

-- stringStrict :: [Char] -> Prs.ParsecT [Char] u DFId.Identity [Char]
-- stringStrict str = do
--   s' <- Prs.string str
--   Prs.notFollowedBy (validIdCharacter) <?> "only whitespace characters."
--   return s'

-- reservedWords :: Prs.ParsecT [Char] u DFId.Identity [Char]
-- reservedWords =
--   Prs.choice (Prs.try <$> stringStrict <$> ["fish", "fin", "swim", "True", "False", "lamprey"])

-- identifier :: SakanaTokenParser u
-- identifier = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.notFollowedBy reservedWords <?> "non-reserved identifier."
--   idPre <- Prs.many1 validIdCharacter
--   idPost <- Prs.many validIdPostId
--   let combinedIdStr = idPre ++ (concat idPost)
--   (return . flip PacketUnit ln . Data . D.Id) combinedIdStr

-- validIdCharacter :: Prs.ParsecT [Char] u DFId.Identity Char
-- validIdCharacter =
--   Prs.letter <|> Prs.char '_' <|> Prs.char '\''
--     <?> "an identifier consisting of \
--         \alphanumeric characters, \'_\', or \'\\'\'"

-- validIdPostId = do
--   dot <- Prs.count 1 (Prs.char '.')
--   idPost <- Prs.many1 validIdCharacter
--   return (dot ++ idPost)

-- operator :: SakanaTokenParser u
-- operator = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   opString <- singleCharOp <|> eq <|> nEqOrDiv <|> someThanOrEqual
--   (return . flip PacketUnit ln . Operator . O.readOp) opString
--   where
--     singleCharOp :: Prs.ParsecT [Char] u DFId.Identity [Char]
--     singleCharOp = do
--       singleOp <- Prs.oneOf ['+', '-', '*', '^']
--       return [singleOp]
--     eq :: Prs.ParsecT [Char] u DFId.Identity [Char]
--     eq = Prs.string "=="
--     nEqOrDiv :: Prs.ParsecT [Char] u DFId.Identity [Char]
--     nEqOrDiv = do
--       div <- Prs.char '/'
--       nEq <- Prs.optionMaybe (Prs.char '=')
--       let nEqOrDiv' = div : DMaybe.maybe [] (: []) nEq
--       return nEqOrDiv'
--     someThanOrEqual :: Prs.ParsecT [Char] u DFId.Identity [Char]
--     someThanOrEqual = do
--       gtOrLt <- Prs.char '>' <|> Prs.char '<'
--       Prs.notFollowedBy (Prs.char '(')
--       eq <- Prs.optionMaybe (Prs.char '=')
--       let someThanOrEqual' = gtOrLt : DMaybe.maybe [] (: []) eq
--       return someThanOrEqual'

-- bracketSendOpen :: SakanaTokenParser u
-- bracketSendOpen = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.char '>'
--   Prs.char '('
--   (return . flip PacketUnit ln . Bracket B.Send) B.Open <?> "open send fish '>('"

-- bracketReturnOpen :: SakanaTokenParser u
-- bracketReturnOpen = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.char '<'
--   Prs.char '('
--   (return . flip PacketUnit ln . Bracket B.Return) B.Open <?> "open return fish '>('"

-- bracketSendClose :: SakanaTokenParser u
-- bracketSendClose = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.char ')'
--   Prs.char '>'
--   (return . flip PacketUnit ln . Bracket B.Send) B.Close <?> "closed send fish '>('"

-- bracketReturnClose :: SakanaTokenParser u
-- bracketReturnClose = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.char ')'
--   Prs.char '<'
--   (return . flip PacketUnit ln . Bracket B.Return) B.Close <?> "closed return fish '>('"

-- fish :: SakanaTokenParser u
-- fish = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.string "fish"
--   Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
--   return (PacketUnit (Keyword K.Fish) ln) <?> "the keyword 'fish'"

-- swim :: SakanaTokenParser u
-- swim = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.string "swim"
--   Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
--   return (PacketUnit (Keyword K.Swim) ln) <?> "the keyword 'swim'"

-- fin :: Prs.ParsecT [Char] u DFId.Identity TokenUnit
-- fin = do
--   pos <- Prs.getPosition
--   let ln = Prs.sourceLine pos
--   Prs.string "fin"
--   Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
--   return (PacketUnit (Control C.Fin) (ln)) <?> "the keyword 'fin'"

-- ----Units to trees parsers----------------------------------------------------------------

-- foldAppendChildren :: Foldable t => Tree.Tree a -> t [Tree.Tree a] -> Tree.Tree a
-- foldAppendChildren toApp toFold = DList.foldl' (-<=) toApp toFold

-- maybeSpaced :: SakanaTreeParser u -> SakanaTreeParser u
-- maybeSpaced p = do
--   p' <- p
--   Prs.spaces
--   return p'

-- tokenUnitToTree :: B.ScopeType -> TokenUnit -> SyntaxTree
-- tokenUnitToTree st = Tree.tree . flip tokenUnitToSyntaxUnit st

-- nullBracket st = do
--   if st == B.Send then bracketSendOpen else bracketReturnOpen
--   Prs.spaces
--   if st == B.Send then bracketSendClose else bracketReturnClose
--   let nullTrees = [(Tree.tree . setContext st . genericSyntaxUnit . Data) D.Null]
--   return nullTrees

-- dataTree :: B.ScopeType -> SakanaTreeParser u
-- dataTree st = do
--   dataToTree <- dataData
--   Prs.spaces
--   let dataTrees = [(tokenUnitToTree st) dataToTree]
--   return dataTrees <?> "a primitive data type, Num, String, or Boolean."

-- idTree :: B.ScopeType -> SakanaTreeParser u
-- idTree st = do
--   idToTree <- identifier
--   Prs.spaces
--   let idTrees = [tokenUnitToTree st idToTree]
--   return idTrees <?> "a valid identifier."

-- bracketContainingExpr :: B.ScopeType -> SakanaTreeParser u
-- bracketContainingExpr st = do
--   if st == B.Send then bracketSendOpen else bracketReturnOpen
--   Prs.spaces
--   expr' <- expr st
--   Prs.spaces
--   if st == B.Send then bracketSendClose else bracketReturnClose
--   Prs.spaces
--   return expr'

-- opExpr :: B.ScopeType -> SakanaTreeParser u
-- opExpr st = do
--   opT <- operator
--   Prs.spaces
--   args <- (Prs.count 2 . bracketContainingExpr) B.Send
--   Prs.spaces
--   let opExprTrees =
--         [((tokenUnitToTree st) opT `foldAppendChildren` args)]
--   return opExprTrees

-- finExpr :: B.ScopeType -> SakanaTreeParser u
-- finExpr st = do
--   f <- fin
--   Prs.spaces
--   args <-
--     (Prs.count 3 . bracketContainingExpr) B.Send
--       <?> "3 required arguments for \
--           \the fin keyword. One boolean expression, one expression to return if true, \
--           \and one expression to return if false."
--   Prs.spaces
--   let finTrees = [(tokenUnitToTree st) f `foldAppendChildren` args]
--   return finTrees

-- fishSend :: SakanaTreeParser u
-- fishSend = do
--   bracketSendOpen
--   Prs.spaces
--   bindTo <- identifier
--   Prs.spaces
--   bracketReturnOpen
--   Prs.spaces
--   valueToBind <- expr B.Return
--   Prs.spaces
--   bracketReturnClose
--   Prs.spaces
--   bracketSendClose
--   Prs.spaces
--   let sendTrees = [(tokenUnitToTree B.Send) bindTo -<= valueToBind]
--   return sendTrees

-- swimExp :: SakanaTreeParser u
-- swimExp = do
--   s <- swim
--   Prs.spaces
--   procs <- Prs.many (Prs.try (bracketContainingExpr B.Send) <|> fishSend)
--   Prs.spaces
--   returnValue <-
--     bracketContainingExpr B.Return
--       <?> "a return fish. A Swim expression \
--           \must return a value, even if that value is null."
--   Prs.spaces
--   let swimTrees =
--         [(tokenUnitToTree B.Return) s `foldAppendChildren` procs -<= returnValue]
--   return swimTrees

-- funcCall :: B.ScopeType -> SakanaTreeParser u
-- funcCall st = do
--   callId <- identifier
--   Prs.spaces
--   args <-
--     Prs.many
--       (Prs.choice (Prs.try <$> [bracketContainingExpr B.Send, nullBracket B.Send]))
--   Prs.spaces
--   let funcCallTrees = [(tokenUnitToTree st) callId `foldAppendChildren` args]
--   return funcCallTrees

-- funcDeclArg :: SakanaTreeParser u
-- funcDeclArg = do
--   bracketSendOpen
--   Prs.spaces
--   argstmnt <- Prs.try (funcDecl B.Send) <|> idTree B.Send
--   Prs.spaces
--   bracketSendClose
--   Prs.spaces
--   return argstmnt <?> "a function argument in the form of a statement."

-- funcDecl :: B.ScopeType -> SakanaTreeParser u
-- funcDecl st = do
--   Prs.spaces
--   f <- Prs.try fish <?> "fish to declare a function."
--   Prs.spaces
--   declId <- identifier
--   Prs.spaces
--   -- declArgs <- Prs.many (Prs.choice (Prs.try <$> [funcDeclArg, nullBracket B.Send]))
--   partialBody <- lampreyExpr
--   Prs.spaces
--   let funcDeclTrees =
--         [ tokenUnitToTree st f
--             -<= [(tokenUnitToTree B.Send) declId]
--             -<= partialBody
--         ]
--   return funcDeclTrees

-- lampreyExpr :: SakanaTreeParser u
-- lampreyExpr = do
--   Prs.spaces
--   Prs.optionMaybe (Prs.string "lamprey")
--   Prs.spaces
--   args <- (Prs.many . Prs.choice . (<$>) Prs.try) [funcDeclArg, nullBracket B.Send]
--   Prs.spaces
--   funcReturn <- Prs.try (bracketContainingExpr B.Return) <|> swimExp
--   Prs.spaces
--   let funcTrees =
--         [ ((tokenUnitToTree B.Return) (PacketUnit (Keyword (K.Lamprey)) 0))
--             -<= ((concat args) ++ funcReturn)
--         ]
--   return funcTrees

-- expr :: B.ScopeType -> SakanaTreeParser u
-- expr st =
--   Prs.choice
--     ( Prs.try
--         <$> [ opExpr st,
--               finExpr st,
--               swimExp,
--               funcCall st,
--               dataTree st,
--               lampreyExpr
--             ]
--     )
--     <?> "Expression, a phrase that can return a value:\
--         \ (fin, swim, a function call, or data)"

-- stmnt :: B.ScopeType -> SakanaTreeParser u
-- stmnt st =
--   funcDecl st
--     <?> "Statement, a phrase that cannot be executed to return a\
--         \ value a function declaration."

-- program :: SakanaTreeParser u
-- program = do
--   stmnts <- (Prs.many . maybeSpaced . stmnt) B.Return <?> "Statements"
--   Prs.spaces
--   toExecute <- Prs.optionMaybe maybeHasExecute
--   let justExecuteTree = DMaybe.fromMaybe [] toExecute
--   let progTree =
--         [ (tokenUnitToTree B.Return)
--             (PacketUnit (Data (D.Id "main")) 0)
--             `foldAppendChildren` stmnts -<= justExecuteTree
--         ]
--   return progTree
--   where
--     maybeHasExecute =
--       Prs.choice
--         (Prs.try <$> [bracketContainingExpr B.Return, swimExp, expr B.Return])

-- runSakanaParser :: Prs.SourceName -> [Char] -> Either Prs.ParseError [SyntaxTree]
-- runSakanaParser srcName contents = do
--   eitherDocTreeOrError <- Prs.runParser program () srcName contents
--   return eitherDocTreeOrError

-- generateSyntaxTreeMain :: Prs.SourceName -> [Char] -> SyntaxTree
-- generateSyntaxTreeMain src = (head . DEither.fromRight [Tree.Empty] . runSakanaParser src)

-- generateSyntaxTree :: [Char] -> SyntaxTree
-- generateSyntaxTree str = head $ DEither.fromRight ([Tree.Empty]) (runSakanaParser "" str)

-- s' =
--   "< >(5)> >(0)>"

-- pt :: SakanaTreeParser () -> [Char] -> IO ()
-- pt p s = Prs.parseTest p s

-- test = Prs.parseTest program s'

-- window :: Int -> [Char] -> [Char]
-- window start = take 20 . drop (start - 9)