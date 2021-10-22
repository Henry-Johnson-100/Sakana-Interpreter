module ExecutionTree
  ( EnvironmentStack (..),
    calct',
    executeMain,
    execute,
    getMainExecutionTrees,
    getMainEnvironmentStack,
    --Anything below this is a temporary export
    emptyEnvironmentStack,
    getFuncDeclArgs,
    getFunctionDeclPositionalArgs,
  )
where

--           ███
--         ██░  ██
--         █░░   █                      ███
--        █▒░░    █                    █░  █
--        █▒░░    █                   █▒░   █
-- ██     █▒▒░░░░░█                   █▒░   █
-- ████    █▒▒░░░█                    █▒░░░░█
--  █ ██   ██▒▒▒██                     █▒▒░█
--  █   █    ███           ███          ███        █████
--  █   ██                 █░███                  █▒░   █
--  █    █                 ██░ ██                █▒░░    █
--  █∙∙   █                 ██  ██               █▒░░    █
--  █  ∙  █                  █▒  ██              █▒░░░░░░█
--   █ ∙  ██                 █▒░  ██              █▒▒░░░█
--   █ ∙∙  ██               █▒▒░░  █               █████
--   █░ ∙   █            ███████████████
--   █░ ∙∙   █      █████               █████
--   █░░ ∙∙  ██ ████        ∙                ████
--    █░  ∙∙  ██                    ∙  ∙ ∙       ██
--    █∙∙  ∙∙█     ∙     ∙     ∙                █  █
--    █░ ∙∙∙█     ∙   ∙     ∙       ∙ ∙   ∙  ∙ █ █  █
--    █░ ∙∙█░░░░░        ∙     ∙  ∙         ∙   █    █
--    █∙∙∙ █▒▒▒░░░░                 ∙              ∙ █
--    █░∙∙▓█▓▓▒▒▒░░░░░                   ░░░█        █
--    █░∙ ▓▓█▓▓▓▒▒░░░░░░░░░░░░░░░░     ░░░░░█       █
--    █░  ▓▓██▓▓▓▒▒▒▒░░░░░░░░░░░░░░░░░░░░░░██ ▒▒▒▒▓█
--   █░░▓▓▓██ ██▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒█▓▒▒▒▓██
--   █░▓▓██     ████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓█▓████
--   █▓▓██       ██▓█████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓█████
--   ███         █░░ ▓▓█ ███████████████
--              █     █    █▓▓▓▓▒░░░░██
--              ██████     █▓▓▓▒▒░░ █
--                       ███▓▒▒▒░░  █
--                      ██▒▒▒▒░░░  █
--                     ███░░░░    ██
--                       ██░    ██      █████
--                        ██░████      █░░   █
--                         ███        █░░░    █
--                                   █▒░░░     █
--                                   █▒░░░░    █
--                                   █▒▒░░░░░░░█
--                                    █▒▒░░░░░█
--                                     █▒▒▒▒▒█
--                                      █████

import qualified Data.Char as DChar (isSpace)
import qualified Data.List as DList (find, foldl', intercalate, intersperse, takeWhile)
import Data.Maybe (Maybe (..))
import qualified Data.Maybe as DMaybe (fromJust, fromMaybe, isJust, isNothing, maybe)
import qualified Data.Tuple as DTuple (uncurry)
import qualified Exception.Base as Exception
  ( ExceptionSeverity (Debug, Fatal),
    ExceptionType
      ( General,
        MissingPositionalArguments,
        OperatorTypeError,
        SymbolIsAlreadyBound,
        SymbolNotFound,
        UndefinedOperatorBehavior
      ),
    newException,
    raiseError,
  )
import qualified Lexer
  ( Token (Control, Data, Keyword, Operator),
    TokenUnit,
    baseData,
    dataTokenIsId,
    fromToken,
    genericData,
    genericOperator,
    keywordTokenIsDeclarationRequiringId,
    tokenize,
  )
import SyntaxTree (SyntaxTree, setContext)
import SyntaxTree as SyntaxUnit (SyntaxUnit (..))
import qualified SyntaxTree
  ( SyntaxTree,
    SyntaxUnit (SyntaxUnit, line, token),
    generateSyntaxTree,
    genericSyntaxUnit,
    getSyntaxAttributeFromTree,
  )
import qualified SyntaxTree as SyntaxUnit (SyntaxUnit (context, line, token))
import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe
import qualified Token.Bracket as B (ScopeType (Return, Send))
import qualified Token.Control as C (Control (Fin))
import qualified Token.Data as D
  ( Data (Boolean, Null, Num, String),
    fromData,
    isNumeric,
    isPrimitive,
    unBoolean,
    unNum,
    unString,
  )
import qualified Token.Keyword as K
import qualified Token.Operator as O
  ( Operator (Add, Div, Eq, Gt, GtEq, Lt, LtEq, Mult, NEq, Pow, Sub),
    fromOp,
  )
import qualified Token.Util.Like as LikeClass (Like (like))
import qualified Token.Util.Tree as Tree
  ( Tree (Empty),
    TreeIO (fPrintTree, ioPrintTree),
    maybeOnTreeNode,
    reTree,
    tree,
    treeChildren,
    treeNode,
    (-<-),
    (-<=),
  )

-- | Unsure if this should be an instance but I will keep it for now
class Truthy a where
  truthy :: a -> Bool
  falsy :: a -> Bool

instance Truthy D.Data where
  truthy (D.Num x) = x == 1.0
  truthy (D.String x) = (not . null . filter (not . DChar.isSpace)) x
  truthy (D.Boolean x) = x
  truthy _ = False
  falsy = not . truthy

data SymbolPair = SymbolPair
  { symbolId :: SyntaxUnit,
    symbolVal :: SyntaxTree
  }
  deriving (Show, Eq)

type SymbolTable = [SymbolPair]

type EnvironmentStack = [SymbolTable]

currentStackSymbolTable :: EnvironmentStack -> SymbolTable
currentStackSymbolTable [] = []
currentStackSymbolTable env = head env

enclosingEnvironmentStack :: EnvironmentStack -> EnvironmentStack
enclosingEnvironmentStack [] = []
enclosingEnvironmentStack (st : []) = []
enclosingEnvironmentStack (st : sts) = sts

fPrintEnvironmentStack :: EnvironmentStack -> [Char]
fPrintEnvironmentStack env =
  (DList.intercalate "\n" . map (DList.intercalate "\n" . map fPrintSymbolPair')) env

fPrintSymbolPair' :: SymbolPair -> String
fPrintSymbolPair' (SymbolPair sid tr) =
  concat ["Symbol ID: ", show sid, "\n", Tree.fPrintTree 0 tr]

emptyEnvironmentStack :: EnvironmentStack
emptyEnvironmentStack = []

--Functions to manage scope and environments----------------------------------------------
------------------------------------------------------------------------------------------

-- | The first environment is prepended to the second,
-- meaning it is enclosed in the second.
encloseEnvironmentIn :: EnvironmentStack -> EnvironmentStack -> EnvironmentStack
encloseEnvironmentIn envInner envOuter = envInner ++ envOuter

addSymbolToEnvironmentStack :: EnvironmentStack -> SymbolPair -> EnvironmentStack
addSymbolToEnvironmentStack [] symPair = [[symPair]]
addSymbolToEnvironmentStack env symPair =
  (symPair : (currentStackSymbolTable env)) : (enclosingEnvironmentStack env)

addTableToEnvironmentStack :: EnvironmentStack -> SymbolTable -> EnvironmentStack
addTableToEnvironmentStack [] symTable = [symTable]
addTableToEnvironmentStack env symTable = symTable : env

lookupSymbolInEnvironmentStack :: EnvironmentStack -> SyntaxUnit -> SymbolPair
lookupSymbolInEnvironmentStack env lookupId =
  DMaybe.fromMaybe
    (symbolNotFoundError lookupId)
    (maybeLookupSymbolInEnvironmentStack env lookupId)

maybeLookupSymbolInEnvironmentStack :: EnvironmentStack -> SyntaxUnit -> Maybe SymbolPair
maybeLookupSymbolInEnvironmentStack [] _ = Nothing
maybeLookupSymbolInEnvironmentStack (st : sts) lookupId =
  if DMaybe.isNothing maybeSymbolInCurrentTable
    then maybeLookupSymbolInEnvironmentStack sts lookupId
    else maybeSymbolInCurrentTable
  where
    maybeSymbolInCurrentTable = maybeLookupSymbolInSymbolTable lookupId st

makeEnvironmentStackFrame :: [SyntaxTree] -> EnvironmentStack
makeEnvironmentStackFrame = (: emptyEnvironmentStack) . makeSymbolTable

makeSymbolTable :: [SyntaxTree] -> SymbolTable
makeSymbolTable = makeSymbolTable' []

makeSymbolTable' :: SymbolTable -> [SyntaxTree] -> SymbolTable
makeSymbolTable' st [] = st
makeSymbolTable' st (tr : trs) =
  DMaybe.maybe
    (makeSymbolTable' st trs)
    (flip makeSymbolTable' trs . (: st))
    (maybeTreeToSymbolPair st tr)

maybeTreeToSymbolPair :: SymbolTable -> SyntaxTree -> Maybe SymbolPair
maybeTreeToSymbolPair st tr' =
  if treeIsStoreable tr'
    then (Just . checkForSameScopeAssignment st . makeSymbolPair) tr'
    else Nothing

checkForSameScopeAssignment :: SymbolTable -> SymbolPair -> SymbolPair
checkForSameScopeAssignment [] sp = sp
checkForSameScopeAssignment st sp =
  if (DMaybe.isNothing . flip maybeLookupSymbolInSymbolTable st . symbolId) sp
    then sp
    else
      symbolAlreadyExistsException
        (symbolId sp)
        ((DMaybe.fromJust . flip maybeLookupSymbolInSymbolTable st . symbolId) sp)

symbolAlreadyExistsException :: SyntaxUnit -> SymbolPair -> a2
symbolAlreadyExistsException lookupId existingSymbol =
  Exception.raiseError $
    Exception.newException
      Exception.SymbolIsAlreadyBound
      [line lookupId, (line . symbolId) existingSymbol]
      ( "The symbol: \'"
          ++ (Lexer.fromToken . token) lookupId
          ++ "\' Already exists in the current scope and is bound to the symbol entry:\n"
          ++ fPrintSymbolPair' existingSymbol
      )
      Exception.Fatal

symbolNotFoundError :: SyntaxUnit -> a2
symbolNotFoundError lookupId =
  Exception.raiseError $
    Exception.newException
      Exception.SymbolNotFound
      [SyntaxUnit.line lookupId]
      ( "A value binding with the Id, \'"
          ++ ((Lexer.fromToken . SyntaxUnit.token) lookupId)
          ++ "\' does not exist in the current scope."
      )
      Exception.Fatal

maybeLookupSymbolInSymbolTable ::
  SyntaxUnit ->
  SymbolTable ->
  Maybe SymbolPair
maybeLookupSymbolInSymbolTable lookupId =
  DList.find (((SyntaxUnit.token) lookupId ==) . SyntaxUnit.token . symbolId)

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
  | treeIsSymbolValueBinding tr = SymbolPair ((DMaybe.fromJust . Tree.treeNode) tr) tr
  where
    declId tr =
      DMaybe.fromMaybe
        (SyntaxTree.genericSyntaxUnit (Lexer.Data D.Null))
        ((head' . Tree.treeChildren) tr >>= Tree.treeNode)

--Evaluation functions used to take a tree and return some FISH value.--------------------
------------------------------------------------------------------------------------------
getMainEnvironmentStack :: SyntaxTree -> EnvironmentStack
getMainEnvironmentStack = makeEnvironmentStackFrame . Tree.treeChildren

-- -- | Gets the last tree in 'main' that can be executed.
-- getMainExecutionTree :: SyntaxTree -> SyntaxTree
-- getMainExecutionTree =
--   DMaybe.fromMaybe ((Tree.tree . SyntaxTree.genericSyntaxUnit) (Lexer.Data (D.Null)))
--     . last'
--     . filter (treeIsExecutable)
--     . Tree.treeChildren

getMainExecutionTrees :: SyntaxTree -> [SyntaxTree]
getMainExecutionTrees docTree =
  DMaybe.maybe [] Tree.treeChildren ((DList.find treeIsSwim . Tree.treeChildren) docTree)

-- | This will be our new main entry point for a fish program
-- If no execution tree can be found in 'main' then a single tree containing a null value
-- will be returned.
executeMain :: IO EnvironmentStack -> IO [SyntaxTree] -> IO D.Data
executeMain envio trio = do
  tr <- trio
  env <- envio
  procExecute (env) (tr)

-- Will cease execution and return at the first return context it sees
procExecute :: EnvironmentStack -> [SyntaxTree] -> IO D.Data
procExecute _ [] = return D.Null
procExecute env (tr : trs)
  | treeIsSendingValueBinding tr = do
    fishSendEnv <- fishSend env tr
    procExecute fishSendEnv trs
  | nodeStrictlySatisfies ((B.Send ==) . SyntaxUnit.context) tr =
    execute env tr >> procExecute env trs
  | nodeStrictlySatisfies ((B.Return ==) . SyntaxUnit.context) tr = execute env tr
  | otherwise = return D.Null

execute :: EnvironmentStack -> SyntaxTree -> IO D.Data
execute env tr
  | nodeStrictlySatisfies nodeIsDataTokenAndPrimitive tr = evaluatePrimitiveData tr
  | treeIsSimpleValueBindingCall tr = 
    execute env ((DMaybe.fromJust . head' . Tree.treeChildren . symbolVal . lookupSymbolInEnvironmentStack env . (DMaybe.fromJust . Tree.treeNode)) tr) --Will require some extensive testing to make sure I'm not totally screwing this up.
  | nodeStrictlySatisfies nodeIsFin tr = evaluateFin env tr
  | nodeStrictlySatisfies nodeIsOperator tr = evaluateOperator env tr
  | treeIsStandardLibCall tr =
    case ( D.fromData
             . DMaybe.fromJust
             . Lexer.baseData
             . SyntaxUnit.token
             . DMaybe.fromJust
             . Tree.treeNode
         )
      tr of
      "trout" ->
        trout
          env
          ((DMaybe.fromJust . head' . Tree.treeChildren) tr)
      "dolphin" -> dolphin
      _ -> return D.Null
  | treeIsFunctionCall tr =
    executeFunctionCall env tr
  | otherwise = return D.Null

evaluatePrimitiveData :: SyntaxTree -> IO D.Data
evaluatePrimitiveData = return . getNodeTokenBaseData

evaluateFin :: EnvironmentStack -> SyntaxTree -> IO D.Data
evaluateFin env tr = do
  cond <- (execute env . DMaybe.fromJust . head') args'
  let forTrue = (DMaybe.fromJust . head' . tail') args'
  let forFalse = (DMaybe.fromJust . head' . tail' . tail') args'
  if truthy cond then execute env forTrue else execute env forFalse
  where
    args' = Tree.treeChildren tr

evaluateOperator :: EnvironmentStack -> SyntaxTree -> IO D.Data
evaluateOperator env tr = do
  let ioOperatorArguments = getOperatorArgs env tr
  argx <- fst ioOperatorArguments
  argy <- snd ioOperatorArguments
  if both D.isNumeric (argx, argy)
    then return $
      case getNodeOperator tr of
        O.Add -> D.Num (justUnNum argx + justUnNum argy)
        O.Sub -> D.Num (justUnNum argx - justUnNum argy)
        O.Mult -> D.Num (justUnNum argx * justUnNum argy)
        O.Div -> D.Num (justUnNum argx / justUnNum argy)
        O.Pow -> D.Num (justUnNum argx ** justUnNum argy)
        --This one doesn't really need to be under 'both isNumeric'
        O.Eq -> D.Boolean (justUnNum argx == justUnNum argy)
        O.NEq -> D.Boolean (justUnNum argx /= justUnNum argy)
        O.Gt -> D.Boolean (justUnNum argx > justUnNum argy)
        O.Lt -> D.Boolean (justUnNum argx < justUnNum argy)
        O.GtEq -> D.Boolean (justUnNum argx >= justUnNum argy)
        O.LtEq -> D.Boolean (justUnNum argx <= justUnNum argy)
    else
      if both (D.String "" `LikeClass.like`) (argx, argy)
        then return $
          case getNodeOperator tr of
            O.Add -> D.String (justUnString argx ++ justUnString argy)
            O.Eq -> D.Boolean (justUnString argx == justUnString argy)
            O.NEq -> D.Boolean (justUnString argx /= justUnString argy)
            O.Gt -> D.Boolean (justUnString argx > justUnString argy)
            O.Lt -> D.Boolean (justUnString argx < justUnString argy)
            O.GtEq -> D.Boolean (justUnString argx >= justUnString argy)
            O.LtEq -> D.Boolean (justUnString argx <= justUnString argy)
            otherOp ->
              undefinedOperatorBehaviorException
                (O.fromOp otherOp)
                (map show [argx, argy])
        else
          if both (D.Boolean True `LikeClass.like`) (argx, argy)
            then return $
              case getNodeOperator tr of
                O.Eq -> D.Boolean (justUnBoolean argx == justUnBoolean argy)
                O.NEq -> D.Boolean (justUnBoolean argx /= justUnBoolean argy)
                O.Gt -> D.Boolean (justUnBoolean argx > justUnBoolean argy)
                O.Lt -> D.Boolean (justUnBoolean argx < justUnBoolean argy)
                O.GtEq -> D.Boolean (justUnBoolean argx >= justUnBoolean argy)
                O.LtEq -> D.Boolean (justUnBoolean argx <= justUnBoolean argy)
                otherOp ->
                  undefinedOperatorBehaviorException
                    (O.fromOp otherOp)
                    (map show [argx, argy])
            else
              return $
                operatorTypeError
                  ((O.fromOp . getNodeOperator) tr)
                  (map show [argx, argy])
  where
    getNodeOperator tr' = case getNodeToken tr' of (Lexer.Operator o) -> o; _ -> O.Eq
    justUnNum = DMaybe.fromJust . D.unNum
    justUnString = DMaybe.fromJust . D.unString
    justUnBoolean = DMaybe.fromJust . D.unBoolean
    undefinedOperatorBehaviorException opString argStrAndType =
      Exception.raiseError $
        Exception.newException
          Exception.UndefinedOperatorBehavior
          [SyntaxTree.getSyntaxAttributeFromTree SyntaxUnit.line tr]
          ( "The operator \'" ++ opString
              ++ "\' does not have defined usage for the types of: \'"
              ++ DList.intercalate "and" argStrAndType
              ++ "\'."
          )
          Exception.Fatal
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

executeFunctionCall :: EnvironmentStack -> SyntaxTree -> IO D.Data
executeFunctionCall mainExEnv functionCall = do
  let preparedStateIO =
        prepareFunctionCallForExecution mainExEnv functionCall functionDeclaration
  newFuncEnv <- fst preparedStateIO
  newFuncProcTrees <- snd preparedStateIO
  procExecute newFuncEnv newFuncProcTrees
  where
    functionDeclaration = calledFunction mainExEnv functionCall

calledFunction :: EnvironmentStack -> SyntaxTree -> SyntaxTree
calledFunction env =
  symbolVal . calledFunctionSymbol env

calledFunctionSymbol :: EnvironmentStack -> Tree.Tree SyntaxUnit -> SymbolPair
calledFunctionSymbol env =
  lookupSymbolInEnvironmentStack env . DMaybe.fromJust . Tree.treeNode

--Boolean comparison functions used primarily for function guards.------------------------
------------------------------------------------------------------------------------------
----On Node-------------------------------------------------------------------------------

nodeStrictlySatisfies :: (a -> Bool) -> Tree.Tree a -> Bool
nodeStrictlySatisfies = Tree.maybeOnTreeNode False

nodeIsDataToken :: SyntaxUnit -> Bool
nodeIsDataToken = LikeClass.like Lexer.genericData . SyntaxUnit.token

nodeIsNull :: SyntaxUnit -> Bool
nodeIsNull (SyntaxUnit.SyntaxUnit (Lexer.Data (D.Null)) _ _) = True
nodeIsNull _ = False

nodeIsDataTokenAndPrimitive :: SyntaxUnit -> Bool
nodeIsDataTokenAndPrimitive =
  foldIdApplicativeOnSingleton
    all
    [ nodeIsDataToken,
      (DMaybe.maybe False D.isPrimitive) . Lexer.baseData . SyntaxUnit.token
    ]

nodeIsId :: SyntaxUnit -> Bool
nodeIsId = Lexer.dataTokenIsId . SyntaxUnit.token

nodeIsOperator :: SyntaxUnit -> Bool
nodeIsOperator = LikeClass.like Lexer.genericOperator . SyntaxUnit.token

nodeIsFin :: SyntaxUnit -> Bool
nodeIsFin = LikeClass.like (Lexer.Control C.Fin) . SyntaxUnit.token

nodeIsDeclarationRequiringId :: SyntaxUnit -> Bool
nodeIsDeclarationRequiringId =
  Lexer.keywordTokenIsDeclarationRequiringId . SyntaxUnit.token

----On Tree-------------------------------------------------------------------------------

-- | For fish code that looks like:
-- 'some_id <(***)<'
-- where '***' is some wildcard value
-- I would like to not have this as a feature in the language to be honest.
treeIsSymbolValueBinding :: SyntaxTree -> Bool
treeIsSymbolValueBinding tr =
  nodeStrictlySatisfies nodeIsId tr
    && firstChildIsReturnContext tr
  where
    firstChildIsReturnContext tr =
      case ((head' . Tree.treeChildren) tr) >>= Tree.treeNode of
        Nothing -> False
        Just x -> ((B.Return ==) . SyntaxUnit.context) x

-- | For a fish like >(some_id <(***)<)>
-- Where some_id should then be bound to the value *** in whatever scope immediately
-- follows.
treeIsSendingValueBinding :: SyntaxTree -> Bool
treeIsSendingValueBinding tr =
  treeIsSymbolValueBinding tr
    && nodeStrictlySatisfies ((B.Send ==) . SyntaxUnit.context) tr

-- | ditto for this, I don't like it that much
treeIsPrimitiveValueBinding :: SyntaxTree -> Bool
treeIsPrimitiveValueBinding =
  foldIdApplicativeOnSingleton
    all
    [ treeIsSymbolValueBinding,
      nodeStrictlySatisfies nodeIsId,
      treeIsPrimitivelyEvaluable . head . Tree.treeChildren
    ]

treeIsPrimitivelyEvaluable :: SyntaxTree -> Bool
treeIsPrimitivelyEvaluable = any id . applyIsPrimitiveEvaluable

-- | A tree is a function call if and only if the base node is an id
-- and it has no return children.
treeIsFunctionCall :: SyntaxTree -> Bool
treeIsFunctionCall tr =
  nodeStrictlySatisfies nodeIsId tr
    && hasNoReturnChildren tr
  where
    hasNoReturnChildren =
      null
        . filter (Tree.maybeOnTreeNode True ((B.Return ==) . SyntaxUnit.context))
        . Tree.treeChildren

treeIsSimpleValueBindingCall :: SyntaxTree -> Bool
treeIsSimpleValueBindingCall tr =
  treeIsFunctionCall tr
    && (null . filter (not . nodeStrictlySatisfies nodeIsNull) . Tree.treeChildren) tr

-- | Can be stored in a symbol table.
--  As of right now, treeIsStoreable and treeIsExecutable are not opposites.
--  because an anonymous function definition is not storeable
--  yet it is also not executable
--  but, named lambda functions: 'x <( >(m)> <(+ >(m)> >(1)>)<' for instance,
--  are storeable and should be stored as normal value bindings.
treeIsStoreable :: SyntaxTree -> Bool
treeIsStoreable = nodeStrictlySatisfies nodeIsDeclarationRequiringId

treeIsExecutable :: SyntaxTree -> Bool
treeIsExecutable Tree.Empty = False
treeIsExecutable tr =
  contextIsReturn tr
    && (treeIsFunctionCall tr || treeIsPrimitivelyEvaluable tr)
  where
    contextIsReturn = nodeStrictlySatisfies ((B.Return ==) . SyntaxUnit.context)

treeIsPositionalArg :: SyntaxTree -> Bool
treeIsPositionalArg tr =
  (null . Tree.treeChildren) tr
    && nodeStrictlySatisfies ((B.Send ==) . SyntaxUnit.context) tr

treeIsStandardLibCall :: SyntaxTree -> Bool
treeIsStandardLibCall tr =
  nodeStrictlySatisfies nodeIsDataToken tr
    && DMaybe.maybe False funcIdInStdLibList (Tree.treeNode tr)
  where
    funcIdInStdLibList =
      flip elem sakanaStandardLibrary
        . D.fromData
        . DMaybe.fromJust
        . Lexer.baseData
        . SyntaxUnit.token

treeIsSwim :: Tree.Tree SyntaxUnit -> Bool
treeIsSwim tr =
  nodeStrictlySatisfies ((Lexer.Keyword (K.Swim) ==) . SyntaxUnit.token) tr

----get information from a tree-----------------------------------------------------------
------------------------------------------------------------------------------------------

getNodeTokenBaseData :: SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (DMaybe.fromMaybe D.Null . (Lexer.baseData . SyntaxUnit.token))

getNodeToken :: SyntaxTree -> Lexer.Token
getNodeToken = Tree.maybeOnTreeNode (Lexer.Data D.Null) SyntaxUnit.token

getOperatorArgs :: EnvironmentStack -> SyntaxTree -> (IO D.Data, IO D.Data)
getOperatorArgs env tr =
  ( (execute env . DMaybe.fromJust . head' . Tree.treeChildren) tr,
    (execute env . DMaybe.fromJust . head' . tail' . Tree.treeChildren) tr
  )

getFuncDeclArgs :: SyntaxTree -> [SyntaxTree]
getFuncDeclArgs =
  filter (nodeStrictlySatisfies (not . nodeIsNull))
    . tail'
    . init'
    . Tree.treeChildren

getFunctionDeclPositionalArgs :: SyntaxTree -> [SyntaxTree]
getFunctionDeclPositionalArgs =
  filter (treeIsPositionalArg) . getFuncDeclArgs

----misc----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

applyIsPrimitiveEvaluable :: SyntaxTree -> [Bool]
-- Apply new boolean functions that operate on a
-- SyntaxUnit to the second list in this function
applyIsPrimitiveEvaluable =
  ( [nodeStrictlySatisfies]
      <*> [ nodeIsOperator,
            nodeIsFin,
            nodeIsDataTokenAndPrimitive
          ]
      <*>
  )
    . listSingleton

-- | This function serves to create the new stack frame
-- and execution trees when a function is called.
prepareFunctionCallForExecution ::
  EnvironmentStack -> SyntaxTree -> SyntaxTree -> (IO EnvironmentStack, IO [SyntaxTree])
prepareFunctionCallForExecution mainExEnv functionCall functionDeclaration = do
  let functionIOEnvironment =
        makeIOEnvFromFuncCall
          mainExEnv
          (Tree.treeChildren functionCall)
          (getBindableArgs functionDeclaration)
  let functionDeclarationProcTrees = getExecutableChildren functionDeclaration
  (functionIOEnvironment, return functionDeclarationProcTrees)

-- where
-- getFunctionDeclarationProcTrees tr =
--   if nodeStrictlySatisfies
--     (Lexer.keywordTokenIsDeclarationRequiringId . SyntaxUnit.token)
--     tr
--     then (filter (not . treeIsPositionalArg) . tail' . Tree.treeChildren) tr
--     else (filter (not . treeIsPositionalArg) . Tree.treeChildren) tr
-- This function is required to get all of the execution trees of a function
-- declaration, which is why 'getMainExecutionTrees' is called.
-- Ideally, a function declaration works like a miniature program.
-- So it is only natural that it's execution trees are denoted by the 'swim' keyword.
getExecutableChildren :: SyntaxTree -> [SyntaxTree]
getExecutableChildren tr = getMainExecutionTrees tr

-- This function is required to get and potentially bind all provided bindable
-- information in a function declaration, like extra sub-function declarations.
getBindableArgs :: SyntaxTree -> [SyntaxTree]
getBindableArgs = DList.takeWhile (not . treeIsSwim) . getFuncDeclArgs

makeSymbolTableFromFuncCall ::
  EnvironmentStack -> SymbolTable -> [SyntaxTree] -> [SyntaxTree] -> IO SymbolTable
makeSymbolTableFromFuncCall _ table _ [] = return table
makeSymbolTableFromFuncCall _ table [] dfargs =
  if any treeIsPositionalArg dfargs
    then missingPositionalArgumentException dfargs
    else
      ( return
          . flip (++) table
          . map DMaybe.fromJust
          . filter (not . DMaybe.isNothing)
          . map (maybeTreeToSymbolPair table)
      )
        dfargs
  where
    missingPositionalArgumentException :: [Tree.Tree SyntaxUnit] -> a2
    missingPositionalArgumentException fdas =
      Exception.raiseError $
        Exception.newException
          Exception.MissingPositionalArguments
          (map (Tree.maybeOnTreeNode 0 SyntaxUnit.line) fdas)
          ( "Missing positional arguments:\n"
              ++ (unlines . map (Tree.maybeOnTreeNode "N/A" (show))) fdas
          )
          Exception.Fatal
makeSymbolTableFromFuncCall mainExEnv table (cfarg : cfargs) (dfarg : dfargs)
  | treeIsPositionalArg dfarg = do
    cfargVal <- execute mainExEnv cfarg
    argValBinding <- return $ createSymbolPairFromArgTreePair dfarg cfargVal
    makeSymbolTableFromFuncCall mainExEnv (argValBinding : table) cfargs dfargs
  | otherwise = do
    let fromJustSymbolTable =
          DMaybe.maybe table (: table) (maybeTreeToSymbolPair table dfarg)
    makeSymbolTableFromFuncCall mainExEnv (fromJustSymbolTable) cfargs dfargs

createSymbolPairFromArgTreePair :: SyntaxTree -> D.Data -> SymbolPair
createSymbolPairFromArgTreePair dfarg' cfargVal' =
  makeSymbolPair $
    (Tree.tree . DMaybe.fromJust . Tree.treeNode) dfarg'
      Tree.-<- ( Tree.tree
                   . SyntaxTree.setContext B.Return
                   . SyntaxTree.genericSyntaxUnit
                   . Lexer.Data
               )
        cfargVal'

makeIOEnvFromFuncCall ::
  EnvironmentStack ->
  [SyntaxTree] ->
  [SyntaxTree] ->
  IO EnvironmentStack
makeIOEnvFromFuncCall mainExEnv cfargs dfargs = do
  newSubScope <- makeSymbolTableFromFuncCall mainExEnv [] cfargs dfargs
  return (newSubScope : mainExEnv)

----Standard Library Functions------------------------------------------------------------
------------------------------------------------------------------------------------------
sakanaStandardLibrary :: [String]
sakanaStandardLibrary = ["trout", "dolphin"]

sakanaPrint :: D.Data -> IO ()
sakanaPrint = hPutStrLn stdout . D.fromData

trout :: EnvironmentStack -> SyntaxTree -> IO D.Data
trout env toPrint = (execute env toPrint >>= sakanaPrint) >> return D.Null

dolphin :: IO D.Data
dolphin = hGetLine stdin >>= return . D.String

fishSend :: EnvironmentStack -> SyntaxTree -> IO EnvironmentStack
fishSend env encTree = do
  let encrustedSymbolDataIO =
        (execute env . DMaybe.fromJust . head' . Tree.treeChildren) encTree
  encrustedSymbolData <- encrustedSymbolDataIO
  encrustedSymbolPair <-
    return $
      createSymbolPairFromArgTreePair (encTree) encrustedSymbolData
  return ([encrustedSymbolPair] : env)

-- Utility functions, including an improved head and tail---------------------------------
------------------------------------------------------------------------------------------

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

head' :: [a] -> Maybe a
head' [] = Nothing
head' xs = (Just . head) xs

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = (Just . last) xs

foldIdApplicativeOnSingleton :: ((a1 -> a1) -> [b] -> c) -> [a2 -> b] -> a2 -> c
foldIdApplicativeOnSingleton foldF funcAtoB = foldF id . (funcAtoB <*>) . listSingleton

listSingleton :: a -> [a]
listSingleton x = [x]

----StdLibFunctions-----------------------------------------------------------------------
------------------------------------------------------------------------------------------

----testing-------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

s' :: [Char]
s' =
  "fish test >(pos)> <(swim <(pos)<)< swim <(test >(10)>)<"

t' :: [Lexer.TokenUnit]
t' = Lexer.tokenize s'

pt' :: SyntaxTree
pt' = SyntaxTree.generateSyntaxTree t'

pt'' = Tree.treeChildren pt'

env' = getMainEnvironmentStack pt'

met' = getMainExecutionTrees pt'

calct' :: String -> Tree.Tree SyntaxUnit
calct' =
  head . Tree.treeChildren
    . SyntaxTree.generateSyntaxTree
    . Lexer.tokenize

ex' s = do
  let docTree = SyntaxTree.generateSyntaxTree . Lexer.tokenize $ s
  let mainExEnv = return . getMainEnvironmentStack $ docTree
  let mainExTr = return . getMainExecutionTrees $ docTree
  executeMain mainExEnv mainExTr

-- calc' :: String -> D.Data
-- calc' = evaluateNode . calct'

fishEnv =
  "fish to_bool\
  \ >(x)>\
  \  <(\
  \  fin\
  \  >(x)>\
  \  >(True)>\
  \  >(False)>\
  \ )<\
  \fish and\
  \ >(x)>\
  \ >(y)>\
  \  <(\
  \   fin\
  \  >(x)>\
  \ >(to_bool >(y)>)>\
  \ >(False)>\
  \)<\
  \fish not\
  \ >(x)>\
  \ <(\
  \ fin\
  \ >(to_bool >(x)>)>\
  \  >(False)>\
  \  >(True)>\
  \ )<\
  \fish or\
  \ >(x)>\
  \  >(y)>\
  \<(\
  \  fin\
  \ >(x)>\
  \ >(True)>\
  \  >(to_bool >(y)>)>\
  \ )<"

fishCall str = (fishEnv ++ " swim") ++ ("<(" ++ str ++ ")<")

fenv' = putStrLn . fPrintEnvironmentStack

io' tr = (Tree.ioPrintTree tr)