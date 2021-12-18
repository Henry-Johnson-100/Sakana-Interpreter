module SakanaParser
  ( Token (..),
    PacketUnit (..),
    TokenUnit (..),
    SyntaxTree (..),
    SyntaxUnit (..),
    generateSyntaxTreeMain,
    generateSyntaxTree,
    genericSyntaxUnit,
    getSyntaxAttributeFromTree,
    nthChildMeetsCondition,
    setContext,
    tokenUnitToSyntaxUnit,
    fromToken,
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

import qualified Data.Either as DEither
import qualified Data.Functor.Identity as DFId
import qualified Data.List as DList
import qualified Data.Maybe as DMaybe
{-
For Text.Parsec
Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.
-}
import Text.Parsec ((<?>), (<|>))
import qualified Text.Parsec as Prs
import qualified Token.Bracket as B
import qualified Token.Control as C
import qualified Token.Data as D
import qualified Token.Keyword as K
import qualified Token.Operator as O
import qualified Util.Like as LikeClass
import Util.Tree ((-<=))
import qualified Util.Tree as Tree

data Token
  = Bracket B.ScopeType B.BracketTerminal
  | Control C.Control
  | Data D.Data
  | Keyword K.Keyword
  | Operator O.Operator
  deriving (Show, Read, Eq)

fromToken :: Token -> String
fromToken (Bracket st bt) = B.fromBracket st bt
fromToken (Control control) = C.fromControl control
fromToken (Data d) = D.fromData d
fromToken (Keyword keyword) = K.fromKeyword keyword
fromToken (Operator operator) = O.fromOp operator

instance LikeClass.Like Token where
  like (Bracket _ _) (Bracket _ _) = True
  like (Control _) (Control _) = True
  like (Data _) (Data _) = True
  like (Keyword _) (Keyword _) = True
  like (Operator _) (Operator _) = True
  like _ _ = False
  notLike a b = not $ LikeClass.like a b

baseData :: Token -> DMaybe.Maybe D.Data
baseData (Data d) = DMaybe.Just d
baseData _ = DMaybe.Nothing

getTokenBracketScopeType :: Token -> B.ScopeType
getTokenBracketScopeType (Bracket st _) = st

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

dataTokenIsId :: Token -> Bool
dataTokenIsId (Data (D.Id _)) = True
dataTokenIsId _ = False

keywordTokenIsDeclarationRequiringId :: Token -> Bool
keywordTokenIsDeclarationRequiringId t =
  DMaybe.maybe False (K.isDeclarationRequiringId) (baseKeyword t)

baseKeyword :: Token -> DMaybe.Maybe K.Keyword
baseKeyword (Keyword k) = DMaybe.Just k
baseKeyword _ = DMaybe.Nothing

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

type SakanaTokenParser u = Prs.ParsecT [Char] u DFId.Identity TokenUnit

type SakanaTreeParser u = Prs.ParsecT [Char] u DFId.Identity [SyntaxTree]

genericSyntaxUnit :: Token -> SyntaxUnit
genericSyntaxUnit t = SyntaxUnit t 0 B.Return

getSyntaxAttributeFromTree :: (SyntaxUnit -> b) -> SyntaxTree -> b
getSyntaxAttributeFromTree attr =
  Tree.maybeOnTreeNode ((attr . genericSyntaxUnit . Data) D.Null) (attr)

nthChildMeetsCondition :: Int -> (SyntaxTree -> Bool) -> SyntaxTree -> Bool
nthChildMeetsCondition n f st
  | n < 0 = nthChildMeetsCondition ((length . Tree.treeChildren) st + n) f st
  | n > ((length . Tree.treeChildren) st - 1) = False
  | otherwise = (f . (!! n) . Tree.treeChildren) st

setContext :: B.ScopeType -> SyntaxUnit -> SyntaxUnit
setContext st su = su {context = st}

tokenUnitToSyntaxUnit :: TokenUnit -> B.ScopeType -> SyntaxUnit
tokenUnitToSyntaxUnit tu = SyntaxUnit (unit tu) (unitLine tu)

dataDecimal :: Prs.ParsecT [Char] u DFId.Identity [Char]
dataDecimal = do
  dot <- Prs.char '.'
  decimal <- Prs.many1 Prs.digit
  return (dot : decimal)

dataDouble :: SakanaTokenParser u
dataDouble = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  negative <- Prs.optionMaybe (Prs.char '-')
  num <- (Prs.many1 Prs.digit)
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
  string <- Prs.manyTill ((Prs.choice . (<$>) Prs.try) [removeInvisibleSpacing, unescapeEscapedSpaces, anyCharAsString]) (Prs.char '"')
  (return . flip PacketUnit ln . Data . D.String . concat) string

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
dataData = Prs.choice (Prs.try <$> [dataDouble, dataString, dataBoolean])

stringStrict :: [Char] -> Prs.ParsecT [Char] u DFId.Identity [Char]
stringStrict str = do
  s' <- Prs.string str
  Prs.notFollowedBy (validIdCharacter) <?> "only whitespace characters."
  return s'

reservedWords :: Prs.ParsecT [Char] u DFId.Identity [Char]
reservedWords =
  Prs.choice (Prs.try <$> stringStrict <$> ["fish", "fin", "swim", "True", "False", "lamprey"])

identifier :: SakanaTokenParser u
identifier = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.notFollowedBy reservedWords <?> "non-reserved identifier."
  idPre <- Prs.many1 validIdCharacter
  idPost <- Prs.many validIdPostId
  let combinedIdStr = idPre ++ (concat idPost)
  (return . flip PacketUnit ln . Data . D.Id) combinedIdStr

validIdCharacter :: Prs.ParsecT [Char] u DFId.Identity Char
validIdCharacter =
  Prs.letter <|> Prs.char '_' <|> Prs.char '\''
    <?> "an identifier consisting of \
        \alphanumeric characters, \'_\', or \'\\'\'"

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
    singleCharOp :: Prs.ParsecT [Char] u DFId.Identity [Char]
    singleCharOp = do
      singleOp <- Prs.oneOf ['+', '-', '*', '^']
      return [singleOp]
    eq :: Prs.ParsecT [Char] u DFId.Identity [Char]
    eq = Prs.string "=="
    nEqOrDiv :: Prs.ParsecT [Char] u DFId.Identity [Char]
    nEqOrDiv = do
      div <- Prs.char '/'
      nEq <- Prs.optionMaybe (Prs.char '=')
      let nEqOrDiv' = div : DMaybe.maybe [] (: []) nEq
      return nEqOrDiv'
    someThanOrEqual :: Prs.ParsecT [Char] u DFId.Identity [Char]
    someThanOrEqual = do
      gtOrLt <- Prs.char '>' <|> Prs.char '<'
      Prs.notFollowedBy (Prs.char '(')
      eq <- Prs.optionMaybe (Prs.char '=')
      let someThanOrEqual' = gtOrLt : DMaybe.maybe [] (: []) eq
      return someThanOrEqual'

bracketSendOpen :: SakanaTokenParser u
bracketSendOpen = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '>'
  Prs.char '('
  (return . flip PacketUnit ln . Bracket B.Send) B.Open <?> "open send fish '>('"

bracketReturnOpen :: SakanaTokenParser u
bracketReturnOpen = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char '<'
  Prs.char '('
  (return . flip PacketUnit ln . Bracket B.Return) B.Open <?> "open return fish '>('"

bracketSendClose :: SakanaTokenParser u
bracketSendClose = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char ')'
  Prs.char '>'
  (return . flip PacketUnit ln . Bracket B.Send) B.Close <?> "closed send fish '>('"

bracketReturnClose :: SakanaTokenParser u
bracketReturnClose = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.char ')'
  Prs.char '<'
  (return . flip PacketUnit ln . Bracket B.Return) B.Close <?> "closed return fish '>('"

fish :: SakanaTokenParser u
fish = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "fish"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Keyword K.Fish) ln) <?> "the keyword 'fish'"

swim :: SakanaTokenParser u
swim = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "swim"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Keyword K.Swim) ln) <?> "the keyword 'swim'"

fin :: Prs.ParsecT [Char] u DFId.Identity TokenUnit
fin = do
  pos <- Prs.getPosition
  let ln = Prs.sourceLine pos
  Prs.string "fin"
  Prs.notFollowedBy (Prs.alphaNum <|> Prs.char '.')
  return (PacketUnit (Control C.Fin) (ln)) <?> "the keyword 'fin'"

----Units to trees parsers----------------------------------------------------------------

foldAppendChildren :: Foldable t => Tree.Tree a -> t [Tree.Tree a] -> Tree.Tree a
foldAppendChildren toApp toFold = DList.foldl' (-<=) toApp toFold

maybeSpaced :: SakanaTreeParser u -> SakanaTreeParser u
maybeSpaced p = do
  p' <- p
  Prs.spaces
  return p'

tokenUnitToTree :: B.ScopeType -> TokenUnit -> SyntaxTree
tokenUnitToTree st = Tree.tree . flip tokenUnitToSyntaxUnit st

nullBracket st = do
  if st == B.Send then bracketSendOpen else bracketReturnOpen
  Prs.spaces
  if st == B.Send then bracketSendClose else bracketReturnClose
  let nullTrees = [(Tree.tree . setContext st . genericSyntaxUnit . Data) D.Null]
  return nullTrees

dataTree :: B.ScopeType -> SakanaTreeParser u
dataTree st = do
  dataToTree <- dataData
  Prs.spaces
  let dataTrees = [(tokenUnitToTree st) dataToTree]
  return dataTrees <?> "a primitive data type, Num, String, or Boolean."

idTree :: B.ScopeType -> SakanaTreeParser u
idTree st = do
  idToTree <- identifier
  Prs.spaces
  let idTrees = [tokenUnitToTree st idToTree]
  return idTrees <?> "a valid identifier."

bracketContainingExpr :: B.ScopeType -> SakanaTreeParser u
bracketContainingExpr st = do
  if st == B.Send then bracketSendOpen else bracketReturnOpen
  Prs.spaces
  expr' <- expr st
  Prs.spaces
  if st == B.Send then bracketSendClose else bracketReturnClose
  Prs.spaces
  return expr'

opExpr :: B.ScopeType -> SakanaTreeParser u
opExpr st = do
  opT <- operator
  Prs.spaces
  args <- (Prs.count 2 . bracketContainingExpr) B.Send
  Prs.spaces
  let opExprTrees =
        [((tokenUnitToTree st) opT `foldAppendChildren` args)]
  return opExprTrees

finExpr :: B.ScopeType -> SakanaTreeParser u
finExpr st = do
  f <- fin
  Prs.spaces
  args <-
    (Prs.count 3 . bracketContainingExpr) B.Send
      <?> "3 required arguments for \
          \the fin keyword. One boolean expression, one expression to return if true, \
          \and one expression to return if false."
  Prs.spaces
  let finTrees = [(tokenUnitToTree st) f `foldAppendChildren` args]
  return finTrees

fishSend :: SakanaTreeParser u
fishSend = do
  bracketSendOpen
  Prs.spaces
  bindTo <- identifier
  Prs.spaces
  bracketReturnOpen
  Prs.spaces
  valueToBind <- expr B.Return
  Prs.spaces
  bracketReturnClose
  Prs.spaces
  bracketSendClose
  Prs.spaces
  let sendTrees = [(tokenUnitToTree B.Send) bindTo -<= valueToBind]
  return sendTrees

swimExp :: SakanaTreeParser u
swimExp = do
  s <- swim
  Prs.spaces
  procs <- Prs.many (Prs.try (bracketContainingExpr B.Send) <|> fishSend)
  Prs.spaces
  returnValue <-
    bracketContainingExpr B.Return
      <?> "a return fish. A Swim expression \
          \must return a value, even if that value is null."
  Prs.spaces
  let swimTrees =
        [(tokenUnitToTree B.Return) s `foldAppendChildren` procs -<= returnValue]
  return swimTrees

funcCall :: B.ScopeType -> SakanaTreeParser u
funcCall st = do
  callId <- identifier
  Prs.spaces
  args <-
    Prs.many
      (Prs.choice (Prs.try <$> [bracketContainingExpr B.Send, nullBracket B.Send]))
  Prs.spaces
  let funcCallTrees = [(tokenUnitToTree st) callId `foldAppendChildren` args]
  return funcCallTrees

funcDeclArg :: SakanaTreeParser u
funcDeclArg = do
  bracketSendOpen
  Prs.spaces
  argstmnt <- Prs.try (funcDecl B.Send) <|> idTree B.Send
  Prs.spaces
  bracketSendClose
  Prs.spaces
  return argstmnt <?> "a function argument in the form of a statement."

funcDecl :: B.ScopeType -> SakanaTreeParser u
funcDecl st = do
  Prs.spaces
  f <- Prs.try fish <?> "fish to declare a function."
  Prs.spaces
  declId <- identifier
  Prs.spaces
  -- declArgs <- Prs.many (Prs.choice (Prs.try <$> [funcDeclArg, nullBracket B.Send]))
  partialBody <- lampreyExpr
  Prs.spaces
  let funcDeclTrees =
        [ tokenUnitToTree st f
            -<= [(tokenUnitToTree B.Send) declId]
            -<= partialBody
        ]
  return funcDeclTrees

lampreyExpr :: SakanaTreeParser u
lampreyExpr = do
  Prs.spaces
  Prs.optionMaybe (Prs.string "lamprey")
  Prs.spaces
  args <- (Prs.many . Prs.choice . (<$>) Prs.try) [funcDeclArg, nullBracket B.Send]
  Prs.spaces
  funcReturn <- Prs.try (bracketContainingExpr B.Return) <|> swimExp
  Prs.spaces
  let funcTrees =
        [ ((tokenUnitToTree B.Return) (PacketUnit (Keyword (K.Lamprey)) 0))
            -<= ((concat args) ++ funcReturn)
        ]
  return funcTrees

expr :: B.ScopeType -> SakanaTreeParser u
expr st =
  Prs.choice
    ( Prs.try
        <$> [ opExpr st,
              finExpr st,
              swimExp,
              funcCall st,
              dataTree st,
              lampreyExpr
            ]
    )
    <?> "Expression, a phrase that can return a value:\
        \ (fin, swim, a function call, or data)"

stmnt :: B.ScopeType -> SakanaTreeParser u
stmnt st =
  funcDecl st
    <?> "Statement, a phrase that cannot be executed to return a\
        \ value a function declaration."

program :: SakanaTreeParser u
program = do
  stmnts <- (Prs.many . maybeSpaced . stmnt) B.Return <?> "Statements"
  Prs.spaces
  toExecute <- Prs.optionMaybe maybeHasExecute
  let justExecuteTree = DMaybe.fromMaybe [] toExecute
  let progTree =
        [ (tokenUnitToTree B.Return)
            (PacketUnit (Data (D.Id "main")) 0)
            `foldAppendChildren` stmnts -<= justExecuteTree
        ]
  return progTree
  where
    maybeHasExecute =
      Prs.choice
        (Prs.try <$> [bracketContainingExpr B.Return, swimExp, expr B.Return])

runSakanaParser :: Prs.SourceName -> [Char] -> Either Prs.ParseError [SyntaxTree]
runSakanaParser srcName contents = do
  eitherDocTreeOrError <- Prs.runParser program () srcName contents
  return eitherDocTreeOrError

generateSyntaxTreeMain :: Prs.SourceName -> [Char] -> SyntaxTree
generateSyntaxTreeMain src = (head . DEither.fromRight [Tree.Empty] . runSakanaParser src)

generateSyntaxTree :: [Char] -> SyntaxTree
generateSyntaxTree str = head $ DEither.fromRight ([Tree.Empty]) (runSakanaParser "" str)

s' =
  "< >(5)> >(0)>"

pt :: SakanaTreeParser () -> [Char] -> IO ()
pt p s = Prs.parseTest p s

test = Prs.parseTest program s'

window :: Int -> [Char] -> [Char]
window start = take 20 . drop (start - 9)