module ExecutionTree
  ( -- calc',
    calct',
    evaluateNode,
    executeMain,
    --Anything below this is a temporary export
    disambiguateFunction,
    noEnv,
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
import qualified Data.List as DList (find, foldl', intercalate, intersperse, singleton)
import Data.Maybe (Maybe (..))
import qualified Data.Maybe as DMaybe (fromJust, fromMaybe, isJust, isNothing, maybe)
import qualified Data.Tuple as DTuple (uncurry)
import qualified Exception.Base as Exception
import qualified Lexer
import SyntaxTree (SyntaxTree)
import SyntaxTree as SyntaxUnit (SyntaxUnit)
import qualified SyntaxTree
import qualified SyntaxTree as SyntaxUnit (SyntaxUnit (context, line, token))
import System.Environment
import System.IO
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
  truthy (D.String x) =
    not $ foldIdApplicativeOnSingleton any [not . all DChar.isSpace, not . null] x
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

fPrintExecEnv :: ExecEnv -> IO ()
fPrintExecEnv env = putStrLn $ fPrintExecEnv' env
  where
    fPrintExecEnv' :: ExecEnv -> String
    fPrintExecEnv' (ExecEnv encl table) =
      concat
        [ DMaybe.maybe "No enclosing env." fPrintExecEnv' encl,
          "\n",
          (concat . DList.intersperse "\n" . map fPrintSymbolPair') table
        ]
    fPrintSymbolPair' :: SymbolPair -> String
    fPrintSymbolPair' (SymbolPair sid tr) =
      concat ["Symbol ID: ", show sid, "\n", Tree.fPrintTree 0 tr]

noEnv :: ExecEnv
noEnv = ExecEnv Nothing []

--Functions to manage scope and environments----------------------------------------------
------------------------------------------------------------------------------------------

-- | Enclose the first ExecEnv in the second.
encloseEnvIn :: ExecEnv -> ExecEnv -> ExecEnv
encloseEnvIn (ExecEnv _ st) envOuter = ExecEnv (Just envOuter) st

-- | Given an environment and a new symbol pair.
-- Prepend the symbol pair onto the existing environment's symbol table
addSymbol :: ExecEnv -> SymbolPair -> ExecEnv
addSymbol env symPair =
  ExecEnv (execEnvInclosedIn env) (symPair : (execEnvSymbolTable env))

-- | Lookup a symbol in the provided environment using a given SyntaxUnit as a reference.
-- A symbol pair is returned if a symbol id containing an equal token is found.
lookupSymbol :: ExecEnv -> SyntaxUnit -> SymbolPair
lookupSymbol (ExecEnv (Just env) table) lookupId =
  DMaybe.fromMaybe
    (lookupSymbol env lookupId)
    (maybeFindSyntaxUnitWithMatchingTokenInSymbolTable lookupId table)
lookupSymbol (ExecEnv Nothing table) lookupId =
  DMaybe.fromMaybe
    symbolNotFoundError
    (maybeFindSyntaxUnitWithMatchingTokenInSymbolTable lookupId table)
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

maybeFindSyntaxUnitWithMatchingTokenInSymbolTable ::
  Foldable t =>
  SyntaxUnit ->
  t SymbolPair ->
  Maybe SymbolPair
maybeFindSyntaxUnitWithMatchingTokenInSymbolTable lookupId =
  DList.find $ ((SyntaxUnit.token) lookupId ==) . SyntaxUnit.token . symbolId

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

makeExecEnv :: [SyntaxTree] -> ExecEnv
makeExecEnv = DList.foldl' treeToMaybeEnvFold noEnv
  where
    treeToMaybeEnvFold env' tr' =
      if treeIsStoreable tr'
        then ((addSymbol env') . makeSymbolPair) tr'
        else env'

--Evaluation functions used to take a tree and return some FISH value.--------------------
------------------------------------------------------------------------------------------

getMainEnv :: SyntaxTree -> ExecEnv
getMainEnv = (makeExecEnv . Tree.treeChildren)

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
executeMain :: Tree.Tree SyntaxUnit -> D.Data
executeMain tr = execute (getMainEnv tr) (getMainExecutionTree tr)

execute :: ExecEnv -> SyntaxTree -> D.Data
execute env tr
  | treeIsPrimitivelyEvaluable tr = evaluateNode env tr
  | treeIsSymbolValueBinding tr =
    (evaluateNode env . DMaybe.fromJust . head' . Tree.treeChildren) tr
  --The following guard is a bit of a hack to get simple value bindings to execute
  | foldIdApplicativeOnSingleton
      all
      [ null . Tree.treeChildren,
        not . DMaybe.isNothing . Tree.treeNode
      ]
      tr =
    execute env ((symbolVal . lookupSymbol env . DMaybe.fromJust . Tree.treeNode) tr)
  | otherwise = D.Null

evaluateNode :: ExecEnv -> SyntaxTree -> D.Data
evaluateNode _ Tree.Empty = D.Null
evaluateNode env tr = case applyIsPrimitiveEvaluable tr of
  [True, False, False] -> evaluateOperator env tr
  [False, True, False] -> evaluateFin env tr
  [False, False, True] -> evaluatePrimitiveData tr
  _ ->
    Exception.raiseError $
      Exception.newException
        Exception.General
        []
        ( "The tree: "
            ++ (Tree.fPrintTree 0 tr)
            ++ "Matched too many criteria for evaluation in the function, "
            ++ "\'evaluateNode\' : "
            ++ (show . applyIsPrimitiveEvaluable) tr
        )
        Exception.Fatal

evaluatePrimitiveData :: SyntaxTree -> D.Data
evaluatePrimitiveData = getNodeTokenBaseData

evaluateOperator :: ExecEnv -> SyntaxTree -> D.Data
evaluateOperator env tr
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
    args = getOperatorArgs env tr
    argValGeneric f =
      ( (DMaybe.fromJust . f . fst) args,
        (DMaybe.fromJust . f . snd) args
      )
    numArgVals = argValGeneric D.unNum
    stringArgVals = argValGeneric D.unString
    boolArgVals = argValGeneric D.unBoolean
    getNodeOperator tr' = case getNodeToken tr' of (Lexer.Operator o) -> o; _ -> O.Eq
    uncurryArgsToNumOperator op = D.Num (op `DTuple.uncurry` numArgVals)
    uncurryArgsToStringOperator op = D.String (op `DTuple.uncurry` stringArgVals)
    uncurryArgsToBoolOperator op argVals = D.Boolean (op `DTuple.uncurry` argVals)
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
              ++ DList.intercalate "and" argStrAndType
              ++ "\'."
          )
          Exception.Fatal

evaluateFin :: ExecEnv -> SyntaxTree -> D.Data
evaluateFin env tr = if (truthy . (!! 0)) args then args !! 1 else args !! 2
  where
    args = getTreeChildrenPrimitiveValues env tr

--Boolean comparison functions used primarily for function guards.------------------------
------------------------------------------------------------------------------------------
----On Node-------------------------------------------------------------------------------

nodeStrictlySatisfies :: (a -> Bool) -> Tree.Tree a -> Bool
nodeStrictlySatisfies = Tree.maybeOnTreeNode False

nodeIsDataToken :: SyntaxUnit -> Bool
nodeIsDataToken = LikeClass.like Lexer.genericData . SyntaxUnit.token

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

treeIsSymbolValueBinding :: SyntaxTree -> Bool
treeIsSymbolValueBinding tr =
  nodeStrictlySatisfies nodeIsDataToken tr
    && firstChildIsReturnContext tr
  where
    firstChildIsReturnContext tr =
      case ((head' . Tree.treeChildren) tr) >>= Tree.treeNode of
        Nothing -> False
        Just x -> ((B.Return ==) . SyntaxUnit.context) x

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

----get information from a tree-----------------------------------------------------------
------------------------------------------------------------------------------------------

getNodeTokenBaseData :: SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (DMaybe.fromMaybe D.Null . (Lexer.baseData . SyntaxUnit.token))

getNodeToken :: SyntaxTree -> Lexer.Token
getNodeToken = Tree.maybeOnTreeNode (Lexer.Data D.Null) SyntaxUnit.token

getTreeChildrenPrimitiveValues :: ExecEnv -> SyntaxTree -> [D.Data]
getTreeChildrenPrimitiveValues env = map (evaluateNode env) . Tree.treeChildren

getOperatorArgs :: ExecEnv -> SyntaxTree -> (D.Data, D.Data)
getOperatorArgs env tr =
  ( (getValue . DMaybe.fromJust . head' . Tree.treeChildren) tr,
    (getValue . DMaybe.fromJust . head' . tail' . Tree.treeChildren) tr
  )
  where
    getValue tr =
      if nodeStrictlySatisfies nodeIsDataTokenAndPrimitive tr
        then evaluatePrimitiveData tr
        else execute env tr

getFuncDeclArgs :: SyntaxTree -> [SyntaxTree]
getFuncDeclArgs = tail' . init' . Tree.treeChildren

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
    . DList.singleton

-- Not a fan so far desu, seems like a pretty expensive function
disambiguateFunction :: SyntaxTree -> SyntaxTree -> SyntaxTree
disambiguateFunction funcCall funcDef =
  Tree.reTree funcDef
    Tree.-<= ( (funcId funcDef) :
               (rebindFunctionArgs (Tree.treeChildren funcCall) (getFuncDeclArgs funcDef))
                 ++ [funcBody funcDef]
             )
  where
    funcId = DMaybe.fromJust . head' . Tree.treeChildren
    funcBody = DMaybe.fromJust . last' . Tree.treeChildren

-- | Will bind a function call's arguments to a function declaration's positional
-- arguments, will only rebind while the function declaration has positional args
-- Will ignore any extra positional arguments from the function call.
-- Will raise an error if the function call does not have enough positional args.
rebindFunctionArgs :: [SyntaxTree] -> [SyntaxTree] -> [SyntaxTree]
rebindFunctionArgs _ [] = []
rebindFunctionArgs (fca : fcas) (fda : fdas)
  | treeIsPositionalArg fda = (bindValue fca fda) : rebindFunctionArgs fcas fdas
  | otherwise = fda : rebindFunctionArgs (fca : fcas) fdas
  where
    bindValue trFrom trTo =
      trTo
        Tree.-<- ( Tree.tree
                     . setContext
                     . DMaybe.fromMaybe
                       (SyntaxTree.genericSyntaxUnit (Lexer.Data (D.Null)))
                     . Tree.treeNode
                 )
          trFrom
    setContext (SyntaxTree.SyntaxUnit t _ _) = SyntaxTree.SyntaxUnit t 0 B.Return
rebindFunctionArgs [] fdas
  | (any id . map treeIsPositionalArg) fdas =
    Exception.raiseError $
      Exception.newException
        Exception.MissingPositionalArguments
        (map (Tree.maybeOnTreeNode 0 SyntaxUnit.line) fdas)
        ( "Missing positional arguments:\n"
            ++ (unlines . map (Tree.maybeOnTreeNode "N/A" (show))) fdas
        )
        Exception.Fatal
  | otherwise = fdas

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
foldIdApplicativeOnSingleton foldF funcAtoB = foldF id . (funcAtoB <*>) . DList.singleton

----testing-------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

s' :: [Char]
s' =
  ">(x <(+ >(1)> >(2)>)< )>\n"
    ++ ">(y <(2)< )>\n"
    ++ "<(* \n "
    ++ ">(x)> \n"
    ++ ">(y)>)<"

t' :: [Lexer.TokenUnit]
t' = Lexer.tokenize s'

pt' :: SyntaxTree
pt' = SyntaxTree.generateSyntaxTree t'

pt'' = Tree.treeChildren pt'

env = getMainEnv pt'

met = getMainExecutionTree pt'

calct' :: String -> Tree.Tree SyntaxUnit
calct' =
  head . Tree.treeChildren
    . SyntaxTree.generateSyntaxTree
    . Lexer.tokenize

-- calc' :: String -> D.Data
-- calc' = evaluateNode . calct'

iotest :: IO ()
iotest = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  print . executeMain
    . SyntaxTree.generateSyntaxTree
    . Lexer.tokenize
    $ contents