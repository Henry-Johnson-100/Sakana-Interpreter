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

dataDouble :: ParsecT [Char] u Identity Token
dataDouble = do
  num <- (many1 Prs.digit)
  maybeDecimal <- Prs.optionMaybe dataDecimal
  let justNumStr = DMaybe.maybe (num) (num ++) maybeDecimal
  (return . Data . Num . read) justNumStr

dataString :: ParsecT [Char] u Identity Token
dataString = do
  Prs.char '"'
  string <- Prs.manyTill Prs.anyChar (Prs.char '"')
  (return . Data . String) string

dataBoolean :: ParsecT [Char] u Identity Token
dataBoolean =
  (Prs.string "True" <|> Prs.string "False") >>= (return . Data . Boolean . read)

dataNull :: ParsecT [Char] u Identity Token
dataNull = return (Data D.Null)

dataData :: ParsecT [Char] u Identity Token
dataData = try dataDouble <|> try dataString <|> try dataBoolean

isFish :: ParsecT [Char] u Identity Token
isFish = do
  Prs.string "fish"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (Keyword Fish)
