module ExecutionTree
  ( calc',
    calct',
    evaluateNode,
  )
where

import qualified Data.Char (isSpace)
import qualified Data.List (find, intercalate)
import qualified Data.Maybe (fromJust, fromMaybe, isJust, isNothing, maybe)
import qualified Data.Tuple (uncurry)
import qualified Exception.Base as Exception
import qualified Lexer
import SyntaxTree (SyntaxTree)
import SyntaxTree as SyntaxUnit (SyntaxUnit)
import qualified SyntaxTree
import qualified SyntaxTree as SyntaxUnit (SyntaxUnit (context, line, token))
import qualified Token.Bracket as B
import qualified Token.Control as C
import qualified Token.Data as D
import qualified Token.Keyword as K
import qualified Token.Operator as O
import qualified Token.Util.Like as LikeClass
import qualified Token.Util.Tree as Tree

-- | Unsure if this should be an instance but I will keep it for now
class Truthy a where
  truthy :: a -> Bool
  falsy :: a -> Bool

instance Truthy D.Data where
  truthy (D.Num x) = x == 1.0
  truthy (D.String x) = (not . all Data.Char.isSpace) x && (not . null) x
  truthy (D.Boolean x) = x
  truthy _ = False
  falsy = not . truthy

data SymbolPair = SymbolPair
  { symbolId :: SyntaxUnit,
    symbolVal :: SyntaxTree
  }
  deriving (Show, Eq)

data ExecEnv = ExecEnv
  { execEnvInclosedIn :: Maybe ExecEnv,
    execEnvSymbolTable :: [SymbolPair]
  }
  deriving (Show, Eq)

noEnv :: ExecEnv
noEnv = ExecEnv Nothing []

-- | Enclose the first ExecEnv in the second.
encloseEnvIn :: ExecEnv -> ExecEnv -> ExecEnv
encloseEnvIn (ExecEnv _ st) envOuter = ExecEnv (Just envOuter) st

-- | Given an environment and a new symbol pair.
-- Prepend the symbol pair onto the existing environment's symbol table
addSymbol :: ExecEnv -> SymbolPair -> ExecEnv
addSymbol env symPair =
  ExecEnv (execEnvInclosedIn env) (symPair : (execEnvSymbolTable env))

isStoreable :: SyntaxTree -> Bool
isStoreable tr =
  any
    id
    ( [ nodeStrictlySatisfies nodeIsDeclarationRequiringId,
        treeIsSymbolValueBinding
      ]
        <*> [tr]
    )

-- | Lookup a symbol in the provided environment using a given Token as a reference.
-- A symbol pair is returned if a symbol id containing an equal token is found.
lookupSymbol :: ExecEnv -> SyntaxUnit -> SymbolPair
lookupSymbol env lookupId =
  Data.Maybe.fromMaybe
    symbolNotFoundError
    ( Data.List.find
        ((((SyntaxUnit.token) lookupId ==) . SyntaxUnit.token . symbolId))
        (execEnvSymbolTable env)
    )
  where
    symbolNotFoundError =
      Exception.raiseError $
        Exception.newException
          Exception.SymbolNotFound
          [SyntaxUnit.line lookupId]
          ( "A value binding with the Id, \'"
              ++ ((Lexer.fromToken . SyntaxUnit.token) lookupId)
              ++ "\' does not exist in the current scope."
          )
          Exception.Fatal

-- #TODO

-- | Take a syntax tree and create a symbol pair.
-- Is NOT agnostic, should only be called on trees where it would make sense to create a
-- symbol pair.
makeSymbolPair :: SyntaxTree -> SymbolPair
makeSymbolPair Tree.Empty =
  SymbolPair
    (SyntaxTree.genericSyntaxUnit (Lexer.Data D.Null))
    Tree.Empty
makeSymbolPair tr
  | nodeStrictlySatisfies
      nodeIsDeclarationRequiringId
      tr =
    SymbolPair (declId tr) tr
  | treeIsSymbolValueBinding tr = SymbolPair ((Data.Maybe.fromJust . Tree.treeNode) tr) tr
  where
    declId tr =
      Data.Maybe.fromMaybe
        (SyntaxTree.genericSyntaxUnit (Lexer.Data D.Null))
        ((head' . Tree.treeChildren) tr >>= Tree.treeNode)

s' =
  "fish add >(n)> >(m)> <(+ >(n)> >(m)> )<"
    ++ " fish sub >(n)> >(m)> <(- >(n)> >(m)> )< <(add >(1)> >(1)>)<"

t' = Lexer.tokenize s'

pt' = SyntaxTree.generateSyntaxTree t'

calct' =
  head
    . Tree.treeChildren
    . SyntaxTree.generateSyntaxTree
    . Lexer.tokenize

calc' :: String -> D.Data
calc' = evaluateNode . calct'

treeIsExecutable :: SyntaxTree -> Bool
treeIsExecutable Tree.Empty = False
treeIsExecutable tr =
  nodeStrictlySatisfies
    (not . (Lexer.genericKeyword `LikeClass.like`) . SyntaxUnit.token)
    tr
    && nodeStrictlySatisfies ((B.Return ==) . SyntaxUnit.context) tr
-- treeIsExecutable :: SyntaxTree -> Bool
-- treeIsExecutable Tree.Empty = False
-- treeIsExecutable tr =
--   nodeStrictlySatisfies
--     (not . (Lexer.genericKeyword `LikeClass.like`) . SyntaxUnit.token)
--     tr
--     && nodeStrictlySatisfies ((B.Return ==) . SyntaxUnit.context) tr

--Functions to manage scope and environments

makeExecEnv :: [SyntaxTree] -> ExecEnv
makeExecEnv = Data.List.foldl' treeToMaybeEnvFold noEnv
  where
    treeToMaybeEnvFold env' tr' =
      if isStoreable tr'
        then ((addSymbol env') . makeSymbolPair) tr'
        else env'

--Evaluation functions used to take a tree and return some FISH value.

-- | The main entry point as of right now
evaluateNode :: SyntaxTree -> D.Data
evaluateNode Tree.Empty = D.Null
evaluateNode tr
  | nodeStrictlySatisfies nodeIsDataToken tr
      && (D.isPrimitive . getNodeTokenBaseData) tr =
    evaluatePrimitiveData tr
  | nodeStrictlySatisfies nodeIsOperator tr = evaluateOperator tr
  | nodeStrictlySatisfies nodeIsFin tr = evaluateFin tr
  where
    nodeStrictlySatisfies = Tree.maybeOnTreeNode False

evaluatePrimitiveData :: SyntaxTree -> D.Data
evaluatePrimitiveData = getNodeTokenBaseData

evaluateOperator :: SyntaxTree -> D.Data
evaluateOperator tr
  | both D.isNumeric args = case getNodeOperator tr of
    O.Add -> uncurryArgsToNumOperator (+)
    O.Sub -> uncurryArgsToNumOperator (-)
    O.Mult -> uncurryArgsToNumOperator (*)
    O.Div -> uncurryArgsToNumOperator (/)
    O.Pow -> uncurryArgsToNumOperator (**)
    O.Eq -> uncurryArgsToBoolOperator (==) numArgVals
    O.NEq -> uncurryArgsToBoolOperator (/=) numArgVals
    O.Gt -> uncurryArgsToBoolOperator (>) numArgVals
    O.Lt -> uncurryArgsToBoolOperator (<) numArgVals
    O.GtEq -> uncurryArgsToBoolOperator (>=) numArgVals
    O.LtEq -> uncurryArgsToBoolOperator (<=) numArgVals
  | both (D.String "" `LikeClass.like`) args = case getNodeOperator tr of
    O.Add -> uncurryArgsToStringOperator (++)
    O.Eq -> uncurryArgsToBoolOperator (==) stringArgVals
    O.NEq -> uncurryArgsToBoolOperator (/=) stringArgVals
    O.Gt -> uncurryArgsToBoolOperator (>) stringArgVals
    O.Lt -> uncurryArgsToBoolOperator (<) stringArgVals
    O.GtEq -> uncurryArgsToBoolOperator (>=) stringArgVals
    O.LtEq -> uncurryArgsToBoolOperator (<=) stringArgVals
    otherOp ->
      undefinedOperatorBehaviorException
        (O.fromOp otherOp)
        (map show ([fst, snd] <*> [args]))
  | both (D.Boolean True `LikeClass.like`) args = case getNodeOperator tr of
    O.Eq -> uncurryArgsToBoolOperator (==) boolArgVals
    O.NEq -> uncurryArgsToBoolOperator (/=) boolArgVals
    O.Gt -> uncurryArgsToBoolOperator (>) boolArgVals
    O.Lt -> uncurryArgsToBoolOperator (<) boolArgVals
    O.GtEq -> uncurryArgsToBoolOperator (>=) boolArgVals
    O.LtEq -> uncurryArgsToBoolOperator (<=) boolArgVals
    otherOp ->
      undefinedOperatorBehaviorException
        (O.fromOp otherOp)
        (map show ([fst, snd] <*> [args]))
  | otherwise =
    operatorTypeError ((O.fromOp . getNodeOperator) tr) (map show ([fst, snd] <*> [args]))
  where
    args = getOperatorArgs tr
    argValGeneric f =
      ( (Data.Maybe.fromJust . f . fst) args,
        (Data.Maybe.fromJust . f . snd) args
      )
    numArgVals = argValGeneric D.unNum
    stringArgVals = argValGeneric D.unString
    boolArgVals = argValGeneric D.unBoolean
    getNodeOperator tr' = case getNodeToken tr' of (Lexer.Operator o) -> o; _ -> O.Eq
    uncurryArgsToNumOperator op = D.Num (op `Data.Tuple.uncurry` numArgVals)
    uncurryArgsToStringOperator op = D.String (op `Data.Tuple.uncurry` stringArgVals)
    uncurryArgsToBoolOperator op argVals = D.Boolean (op `Data.Tuple.uncurry` argVals)
    operatorTypeError opString argStrAndType =
      Exception.raiseError $
        Exception.newException
          Exception.OperatorTypeError
          [SyntaxTree.getSyntaxAttributeFromTree SyntaxUnit.line tr]
          ( "The operator \'" ++ opString
              ++ "\' cannot be aaplied to the arguments of incompatible types: "
              ++ unwords argStrAndType
              ++ "."
          )
          Exception.Fatal
    undefinedOperatorBehaviorException opString argStrAndType =
      Exception.raiseError $
        Exception.newException
          Exception.UndefinedOperatorBehavior
          [SyntaxTree.getSyntaxAttributeFromTree SyntaxUnit.line tr]
          ( "The operator \'" ++ opString
              ++ "\' does not have defined usage for the types of: \'"
              ++ Data.List.intercalate "and" argStrAndType
              ++ "\'."
          )
          Exception.Fatal

evaluateFin :: SyntaxTree -> D.Data
evaluateFin tr = if (truthy . (!! 0)) args then args !! 1 else args !! 2
  where
    args = getNodeArgs tr

--Boolean comparison functions used primarily for function guards.

nodeStrictlySatisfies :: (a -> Bool) -> Tree.Tree a -> Bool
nodeStrictlySatisfies = Tree.maybeOnTreeNode False

nodeIsDataToken :: SyntaxUnit -> Bool
nodeIsDataToken = LikeClass.like Lexer.genericData . SyntaxUnit.token

nodeIsOperator :: SyntaxUnit -> Bool
nodeIsOperator = LikeClass.like Lexer.genericOperator . SyntaxUnit.token

nodeIsFin :: SyntaxUnit -> Bool
nodeIsFin = LikeClass.like (Lexer.Control C.Fin) . SyntaxUnit.token

nodeIsDeclarationRequiringId :: SyntaxUnit -> Bool
nodeIsDeclarationRequiringId =
  Lexer.keywordTokenIsDeclarationRequiringId . SyntaxUnit.token

treeIsSymbolValueBinding :: SyntaxTree -> Bool
treeIsSymbolValueBinding tr =
  nodeStrictlySatisfies nodeIsDataToken tr
    && firstChildIsReturnContext tr
  where
    firstChildIsReturnContext tr =
      case ((head' . Tree.treeChildren) tr) >>= Tree.treeNode of
        Nothing -> False
        Just x -> ((B.Return ==) . SyntaxUnit.context) x

nodeIsPrimitiveValue :: SyntaxTree -> Bool
nodeIsPrimitiveValue tr =
  Tree.maybeOnTreeNode False nodeIsDataToken tr
    && (D.isPrimitive . getNodeTokenBaseData) tr

getNodeTokenBaseData :: SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (Data.Maybe.fromMaybe D.Null . (Lexer.baseData . SyntaxUnit.token))

getNodeToken :: SyntaxTree -> Lexer.Token
getNodeToken = Tree.maybeOnTreeNode (Lexer.Data D.Null) SyntaxUnit.token

getNodeArgs :: SyntaxTree -> [D.Data]
getNodeArgs = map evaluateNode . Tree.treeChildren

getOperatorArgs :: SyntaxTree -> (D.Data, D.Data)
getOperatorArgs tr =
  ( (getValue . Data.Maybe.fromJust . head' . Tree.treeChildren) tr,
    (getValue . Data.Maybe.fromJust . head' . tail' . Tree.treeChildren) tr
  )
  where
    getValue tr =
      if nodeIsPrimitiveValue tr
        then evaluatePrimitiveData tr
        else evaluateNode tr

-- Utility functions, including an improved head and tail

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = (Just . head) xs

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs