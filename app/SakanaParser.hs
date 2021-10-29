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

type SakanaTokenParser u = ParsecT [Char] u Identity TokenUnit

type SakanaTreeParser u = ParsecT [Char] u Identity [SyntaxTree]

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
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  negative <- Prs.optionMaybe (Prs.char '-')
  num <- (many1 Prs.digit)
  maybeDecimal <- Prs.optionMaybe dataDecimal
  let justNumStr =
        (DMaybe.maybe [] (\x -> id x : []) negative)
          ++ (DMaybe.maybe (num) (num ++) maybeDecimal)
  (return . flip PacketUnit ln . Data . D.readData) justNumStr

dataString :: SakanaTokenParser u
dataString = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '"'
  string <- Prs.manyTill Prs.anyChar (Prs.char '"')
  (return . flip PacketUnit ln . Data . String) string

dataBoolean :: SakanaTokenParser u
dataBoolean = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  b <- Prs.string "True" <|> Prs.string "False"
  (return . flip PacketUnit ln . Data . D.readData) b

dataNull :: SakanaTokenParser u
dataNull = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  (return . flip PacketUnit ln . Data) D.Null

dataData :: SakanaTokenParser u
dataData = try dataDouble <|> try dataString <|> try dataBoolean

identifier :: SakanaTokenParser u
identifier = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  idPre <- Prs.many1 validIdCharacter
  idPost <- Prs.many validIdPostId
  let combinedIdStr = idPre ++ (concat idPost)
  (return . flip PacketUnit ln . Data . Id) combinedIdStr
  where
    validIdCharacter :: ParsecT [Char] u Identity Char
    validIdCharacter = Prs.alphaNum <|> Prs.char '_' <|> Prs.char '\''
    validIdPostId = do
      dot <- Prs.count 1 (Prs.char '.')
      idPost <- Prs.many1 validIdCharacter
      return (dot ++ idPost)

operator :: SakanaTokenParser u
operator = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  opString <- singleCharOp <|> eq <|> nEqOrDiv <|> someThanOrEqual
  (return . flip PacketUnit ln . Operator . O.readOp) opString
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

bracketSendOpen :: SakanaTokenParser u
bracketSendOpen = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '>'
  Prs.char '('
  (return . flip PacketUnit ln . Bracket B.Send) B.Open

bracketReturnOpen :: SakanaTokenParser u
bracketReturnOpen = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '<'
  Prs.char '('
  (return . flip PacketUnit ln . Bracket B.Return) B.Open

bracketSendClose :: SakanaTokenParser u
bracketSendClose = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char ')'
  Prs.char '<'
  (return . flip PacketUnit ln . Bracket B.Send) B.Close

bracketReturnClose :: SakanaTokenParser u
bracketReturnClose = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char ')'
  Prs.char '<'
  (return . flip PacketUnit ln . Bracket B.Return) B.Close

fish :: SakanaTokenParser u
fish = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "fish"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Keyword Fish) ln)

swim :: SakanaTokenParser u
swim = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "swim"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Keyword Swim) ln)

fin :: ParsecT [Char] u Identity TokenUnit
fin = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "fin"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Control C.Fin) (ln))

expr :: SakanaTreeParser u
expr = return [Tree.Empty]