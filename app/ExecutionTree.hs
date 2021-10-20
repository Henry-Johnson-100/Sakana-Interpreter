module ExecutionTree
  ( EnvironmentStack (..),
    calct',
    -- evaluateNode,
    executeMain,
    execute,
    getMainExecutionTree,
    getMainEnvironmentStack,
    --Anything below this is a temporary export
    noEnvironmentStack,
    -- disambiguateFunction,
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
import qualified Data.List as DList (find, foldl', intercalate, intersperse)
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
  ( Token (Control, Data, Operator),
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
import qualified Token.Bracket as B (ScopeType (Return))
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

noEnvironmentStack :: EnvironmentStack
noEnvironmentStack = []

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

-- lookupSymbolInEnvironmentStack :: EnvironmentStack -> SyntaxUnit -> SymbolPair
-- lookupSymbolInEnvironmentStack (st : []) lookupId =
--   DMaybe.fromMaybe
--     (symbolNotFoundError lookupId)
--     (maybeLookupSymbolInSymbolTable lookupId st)
-- lookupSymbolInEnvironmentStack (st : sts) lookupId =
--   DMaybe.fromMaybe
--     (lookupSymbolInEnvironmentStack sts lookupId)
--     (maybeLookupSymbolInSymbolTable lookupId st)

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
makeEnvironmentStackFrame = flip (:) noEnvironmentStack . makeSymbolTable

makeSymbolTable :: [SyntaxTree] -> SymbolTable
makeSymbolTable = makeSymbolTable' []

makeSymbolTable' :: SymbolTable -> [SyntaxTree] -> SymbolTable
makeSymbolTable' st [] = st
makeSymbolTable' st (tr : trs) =
  DMaybe.maybe
    (makeSymbolTable' st trs)
    (\sp -> makeSymbolTable' (sp : st) trs)
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

-- | Gets the last tree in 'main' that can be executed.
getMainExecutionTree :: SyntaxTree -> SyntaxTree
getMainExecutionTree =
  DMaybe.fromMaybe ((Tree.tree . SyntaxTree.genericSyntaxUnit) (Lexer.Data (D.Null)))
    . last'
    . filter (treeIsExecutable)
    . Tree.treeChildren

-- | This will be our new main entry point for a fish program
-- If no execution tree can be found in 'main' then a single tree containing a null value
-- will be returned.
executeMain :: IO EnvironmentStack -> IO SyntaxTree -> IO D.Data
executeMain envio trio = do
  tr <- trio
  env <- envio
  execute (env) (tr)

execute :: EnvironmentStack -> SyntaxTree -> IO D.Data
execute env tr
  | nodeStrictlySatisfies nodeIsFin tr = evaluateFin env tr
  | nodeStrictlySatisfies nodeIsDataTokenAndPrimitive tr = evaluatePrimitiveData tr
  | nodeStrictlySatisfies nodeIsOperator tr = evaluateOperator env tr
  | foldIdApplicativeOnSingleton
      any
      [treeIsFunctionCall, treeIsSimpleValueBindingCall]
      tr =
    executeFunctionCall env tr
  | otherwise = return D.Null

evaluatePrimitiveData :: SyntaxTree -> IO D.Data
evaluatePrimitiveData = return . getNodeTokenBaseData

evaluateFin :: EnvironmentStack -> SyntaxTree -> IO D.Data
evaluateFin env tr = do
  cond <- (DMaybe.fromJust . head') args'
  forTrue <- (DMaybe.fromJust . head' . tail') args'
  forFalse <- (DMaybe.fromJust . head' . tail' . tail') args'
  if truthy cond then return forTrue else return forFalse
  where
    args' = getTreeChildrenPrimitiveValues env tr

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
executeFunctionCall mainExEnv functionCall =
  DTuple.uncurry
    (executeMain)
    (prepareFunctionCallForExecution mainExEnv functionCall functionDeclaration)
  where
    functionDeclaration = calledFunction mainExEnv functionCall

-- executeFunctionCall :: EnvironmentStack -> SyntaxTree -> IO D.Data
-- executeFunctionCall env tr =
--   execute
--     (encloseEnvironmentIn thisCalledFunctionEnv env)
--     (getMainExecutionTree thisDisambiguatedFunction)
--   where
--     thisCalledFunctionSymbol = calledFunctionSymbol env tr
--     thisCalledFunction = symbolVal thisCalledFunctionSymbol
--     thisCalledFunctionEnv = calledFunctionEnv env tr
--     thisDisambiguatedFunction = disambiguateFunction env tr thisCalledFunction

-- calledFunctionEnv :: EnvironmentStack -> SyntaxTree -> EnvironmentStack
-- calledFunctionEnv env tr =
--   makeEnvironmentStackFrame
--     . Tree.treeChildren
--     $ disambiguateFunction env tr (calledFunction env tr)

calledFunction :: EnvironmentStack -> SyntaxTree -> SyntaxTree
calledFunction env =
  symbolVal . calledFunctionSymbol env

calledFunctionSymbol :: EnvironmentStack -> Tree.Tree SyntaxUnit -> SymbolPair
calledFunctionSymbol env =
  lookupSymbolInEnvironmentStack env . DMaybe.fromJust . Tree.treeNode

-- evaluateNode :: EnvironmentStack -> SyntaxTree -> D.Data
-- evaluateNode _ Tree.Empty = D.Null
-- evaluateNode env tr
--   | otherwise = case applyIsPrimitiveEvaluable tr of
--     [True, False, False] -> evaluateOperator env tr
--     _ ->
--       Exception.raiseError $
--         Exception.newException
--           Exception.General
--           []
--           ( "The tree: "
--               ++ (Tree.fPrintTree 0 tr)
--               ++ "Matched too many criteria for evaluation in the function, "
--               ++ "\'evaluateNode\' : "
--               ++ (show . applyIsPrimitiveEvaluable) tr
--               ++ "\nFor tokens, \'"
--               ++ show tr
--               ++ "\'"
--               ++ "\nIn environment: \n"
--               ++ fPrintEnvironmentStack env
--           )
--           Exception.Fatal

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
  nodeStrictlySatisfies nodeIsId tr
    && foldIdApplicativeOnSingleton
      all
      [ null . Tree.treeChildren,
        not . DMaybe.isNothing . Tree.treeNode
      ]
      tr

-- | Can be stored in a symbol table.
--  As of right now, treeIsStoreable and treeIsExecutable are not opposites.
--  because an anonymous function definition is not storeable
--  yet it is also not executable
--  but, named lambda functions: 'x <( >(m)> <(+ >(m)> >(1)>)<' for instance,
--  are storeable and should be stored as normal value bindings.
treeIsStoreable :: SyntaxTree -> Bool
treeIsStoreable =
  foldIdApplicativeOnSingleton
    any
    [ nodeStrictlySatisfies nodeIsDeclarationRequiringId,
      treeIsSymbolValueBinding
    ]

treeIsExecutable :: SyntaxTree -> Bool
treeIsExecutable Tree.Empty = False
treeIsExecutable tr =
  contextIsReturn tr
    && (treeIsFunctionCall tr || treeIsPrimitivelyEvaluable tr)
  where
    contextIsReturn = nodeStrictlySatisfies ((B.Return ==) . SyntaxUnit.context)

treeIsPositionalArg :: SyntaxTree -> Bool
treeIsPositionalArg = null . Tree.treeChildren

-- treeIsStandardLibCall :: SyntaxTree -> Bool
-- treeIsStandardLibCall tr =
--   nodeStrictlySatisfies nodeIsDataToken tr
--     && DMaybe.maybe False funcIdInStdLibList (Tree.treeNode tr)
--   where
--     funcIdInStdLibList =
--       flip elem sakanaStdLib
--         . D.fromData
--         . DMaybe.fromJust
--         . Lexer.baseData
--         . SyntaxUnit.token

----get information from a tree-----------------------------------------------------------
------------------------------------------------------------------------------------------

getNodeTokenBaseData :: SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (DMaybe.fromMaybe D.Null . (Lexer.baseData . SyntaxUnit.token))

getNodeToken :: SyntaxTree -> Lexer.Token
getNodeToken = Tree.maybeOnTreeNode (Lexer.Data D.Null) SyntaxUnit.token

getTreeChildrenPrimitiveValues :: EnvironmentStack -> SyntaxTree -> [IO D.Data]
getTreeChildrenPrimitiveValues env = map (execute env) . Tree.treeChildren

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

prepareFunctionCallForExecution ::
  EnvironmentStack -> SyntaxTree -> SyntaxTree -> (IO EnvironmentStack, IO SyntaxTree)
prepareFunctionCallForExecution mainExEnv functionCall functionDeclaration =
  ( makeIOEnvFromFuncCall
      mainExEnv
      (Tree.treeChildren functionCall)
      (getFuncDeclArgs functionDeclaration),
    (return . getMainExecutionTree) functionDeclaration
  )

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
          DMaybe.maybe table (\x -> x : table) (maybeTreeToSymbolPair table dfarg)
    makeSymbolTableFromFuncCall mainExEnv (fromJustSymbolTable) cfargs dfargs
  where
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
  "fish add >(x)> >(fish y >()> <(1)<)> <(+ >(x)> >(y >()>)>)< <(add >(1)>)<"

t' :: [Lexer.TokenUnit]
t' = Lexer.tokenize s'

pt' :: SyntaxTree
pt' = SyntaxTree.generateSyntaxTree t'

pt'' = Tree.treeChildren pt'

env' = getMainEnvironmentStack pt'

met' = getMainExecutionTree pt'

calct' :: String -> Tree.Tree SyntaxUnit
calct' =
  head . Tree.treeChildren
    . SyntaxTree.generateSyntaxTree
    . Lexer.tokenize

ex' s = do
  let docTree = SyntaxTree.generateSyntaxTree . Lexer.tokenize $ s
  let mainExEnv = return . getMainEnvironmentStack $ docTree
  let mainExTr = return . getMainExecutionTree $ docTree
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

fishCall str = (fishEnv ++ " ") ++ ("<(" ++ str ++ ")<")

fenv' = putStrLn . fPrintEnvironmentStack

io' tr = (Tree.ioPrintTree tr)