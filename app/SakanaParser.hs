module SakanaParser
  (
  )
where

import Data.Functor.Identity (Identity)
import Data.Maybe as DMaybe
import Text.Parsec as Prs
import Token.Bracket as B
import Token.Control as C
import Token.Data as D
import Token.Keyword as K
import Token.Operator as O
import Util.Tree as Tree

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

data SyntaxUnit = SyntaxUnit
  { token :: Token,
    line :: Int,
    context :: B.ScopeType
  }
  deriving (Show, Eq)

type TokenUnit = PacketUnit Token

type SyntaxTree = Tree.Tree SyntaxUnit

type SakanaTokenParser u = ParsecT [Char] u Identity Token

genericSyntaxUnit :: Token -> SyntaxUnit
genericSyntaxUnit t = SyntaxUnit t 0 B.Return

setContext :: B.ScopeType -> SyntaxUnit -> SyntaxUnit
setContext st su = su {context = st}

tokenUnitToSyntaxUnit :: TokenUnit -> B.ScopeType -> SyntaxUnit
tokenUnitToSyntaxUnit tu = SyntaxUnit (unit tu) (unitLine tu)

dataDecimal :: ParsecT [Char] u Identity [Char]
dataDecimal = do
  dot <- Prs.char '.'
  decimal <- many1 Prs.digit
  return (dot : decimal)

dataDouble :: SakanaTokenParser u
dataDouble = do
  num <- (many1 Prs.digit)
  maybeDecimal <- Prs.optionMaybe dataDecimal
  let justNumStr = DMaybe.maybe (num) (num ++) maybeDecimal
  (return . Data . Num . read) justNumStr

dataString :: SakanaTokenParser u
dataString = do
  Prs.char '"'
  string <- Prs.manyTill Prs.anyChar (Prs.char '"')
  (return . Data . String) string

dataBoolean :: SakanaTokenParser u
dataBoolean =
  (Prs.string "True" <|> Prs.string "False") >>= (return . Data . Boolean . read)

dataNull :: SakanaTokenParser u
dataNull = return (Data D.Null)

dataData :: SakanaTokenParser u
dataData = try dataDouble <|> try dataString <|> try dataBoolean

identifier :: SakanaTokenParser u
identifier = do
  idPre <- Prs.many1 validIdCharacter
  idPost <- Prs.many validIdPostId
  let combinedIdStr = idPre ++ (concat idPost)
  (return . Data . Id) combinedIdStr
  where
    validIdCharacter :: ParsecT [Char] u Identity Char
    validIdCharacter = Prs.alphaNum <|> Prs.char '_' <|> Prs.char '\''
    validIdPostId = do
      dot <- Prs.count 1 (Prs.char '.')
      idPost <- Prs.many1 validIdCharacter
      return (dot ++ idPost)

operator :: SakanaTokenParser u
operator = do
  opString <- singleCharOp <|> eq <|> nEqOrDiv <|> someThanOrEqual
  (return . Operator . O.readOp) opString
  where
    singleCharOp :: ParsecT [Char] u Identity [Char]
    singleCharOp = do
      singleOp <- Prs.oneOf ['+', '-', '*', '^']
      return [singleOp]
    eq :: ParsecT [Char] u Identity [Char]
    eq = Prs.string "=="
    nEqOrDiv :: ParsecT [Char] u Identity [Char]
    nEqOrDiv = do
      div <- Prs.char '/'
      nEq <- optionMaybe (Prs.char '=')
      let nEqOrDiv' = div : DMaybe.maybe [] (: []) nEq
      return nEqOrDiv'
    someThanOrEqual :: ParsecT [Char] u Identity [Char]
    someThanOrEqual = do
      gtOrLt <- Prs.char '>' <|> Prs.char '<'
      Prs.notFollowedBy (Prs.char '(')
      eq <- optionMaybe (Prs.char '=')
      let someThanOrEqual' = gtOrLt : DMaybe.maybe [] (: []) eq
      return someThanOrEqual'

isFish :: SakanaTokenParser u
isFish = do
  Prs.string "fish"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (Keyword Fish)
