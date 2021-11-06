module TreeInterpreter
  ( EnvironmentStack (..),
    -- calct',
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
        GeneralTypeError,
        MissingPositionalArguments,
        OperatorTypeError,
        SymbolIsAlreadyBound,
        SymbolNotFound,
        UndefinedOperatorBehavior
      ),
    newException,
    raiseError,
  )
import SakanaParser (SyntaxTree, setContext)
-- import SakanaParser as SyntaxUnit (SyntaxUnit (..))
import qualified SakanaParser
import qualified SakanaParser as SyntaxUnit (SyntaxUnit (context, line, token))
import System.Environment (getArgs)
import System.IO
import qualified Token.Bracket as B (ScopeType (Return, Send))
import qualified Token.Control as C (Control (Fin))
import qualified Token.Data as D
  ( Data (Boolean, Id, Null, Num, String),
    fromData,
    isNumeric,
    isPrimitive,
    isString,
    readData,
    unBoolean,
    unNum,
    unString,
  )
import qualified Token.Keyword as K
import qualified Token.Operator as O
  ( Operator (Add, Div, Eq, Gt, GtEq, Lt, LtEq, Mult, NEq, Pow, Sub),
    fromOp,
  )
import TreeInterpreter.Environment as Env
  ( EnvironmentStack,
    SymbolPair (symbolVal),
    SymbolTable,
    addSymbolToEnvironmentStack,
    emptyEnvironmentStack,
    fPrintEnvironmentStack,
    lookupSymbolInEnvironmentStack,
    makeEnvironmentStackFrame,
    makeSymbolPair,
    maybeTreeToSymbolPair,
  )
import qualified TreeInterpreter.LocalCheck.NodeIs as Check.NodeIs
  ( dataTokenAndPrimitive,
    fin,
    nullNode,
    operator,
    sendContext,
  )
import qualified TreeInterpreter.LocalCheck.TreeIs as Check.TreeIs
  ( executable,
    functionCall,
    positionalArg,
    sendingValueBinding,
    standardLibCall,
    swim,
  )
import qualified Util.General
  ( both,
    foldIdApplicativeOnSingleton,
    head',
    init',
    last',
    listSingleton,
    tail',
  )
import qualified Util.Like as LikeClass (Like (like))
import qualified Util.Tree as Tree
  ( Tree (Empty, (:-<-:)),
    TreeIO (fPrintTree, ioPrintTree),
    maybeOnTreeNode,
    mutateTreeNode,
    nodeStrictlySatisfies,
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

--Functions to manage scope and environments----------------------------------------------
------------------------------------------------------------------------------------------

--Evaluation functions used to take a tree and return some FISH value.--------------------
------------------------------------------------------------------------------------------
getMainEnvironmentStack :: SakanaParser.SyntaxTree -> EnvironmentStack
getMainEnvironmentStack = makeEnvironmentStackFrame . Tree.treeChildren

-- -- | Gets the last tree in 'main' that can be executed.
-- getMainExecutionTree :: SyntaxTree -> SyntaxTree
-- getMainExecutionTree =
--   DMaybe.fromMaybe ((Tree.tree . SyntaxTree.genericSyntaxUnit) (SakanaParser.Data (D.Null)))
--     . last'
--     . filter (treeIsExecutable)
--     . Tree.treeChildren

getMainExecutionTrees :: SyntaxTree -> [SyntaxTree]
getMainExecutionTrees docTree =
  DMaybe.maybe
    [getLastExecutionTree docTree]
    Tree.treeChildren
    ((DList.find Check.TreeIs.swim . Tree.treeChildren) docTree)
  where
    getLastExecutionTree =
      DMaybe.fromMaybe Tree.Empty
        . Util.General.last'
        . filter (Check.TreeIs.executable)
        . Tree.treeChildren

-- | This will be our new main entry point for a fish program
-- If no execution tree can be found in 'main' then a single tree containing a null value
-- will be returned.
executeMain :: IO EnvironmentStack -> IO [SyntaxTree] -> IO String -> IO D.Data
executeMain envio trio sakanaIOArgs = do
  tr <- trio
  env <- envio
  sakanaArgs <- sakanaIOArgs
  let argsBoundToEnv = bindSakanaArgsToArgs env sakanaArgs
  procExecute (argsBoundToEnv) (tr)
  where
    bindSakanaArgsToArgs :: Env.EnvironmentStack -> String -> Env.EnvironmentStack
    bindSakanaArgsToArgs env args =
      ( addSymbolToEnvironmentStack env . Env.makeSymbolPair
          . (Tree.-<-)
            ( ( Tree.tree
                  . SakanaParser.setContext B.Send
                  . SakanaParser.genericSyntaxUnit
                  . SakanaParser.Data
                  . D.Id
              )
                "_args"
            )
          . Tree.tree
          . SakanaParser.setContext B.Return
          . SakanaParser.genericSyntaxUnit
          . SakanaParser.Data
          . D.String
      )
        args

-- Will cease execution and return at the first return context it sees
procExecute :: EnvironmentStack -> [SyntaxTree] -> IO D.Data
procExecute _ [] = return D.Null
procExecute env (tr : trs)
  | Check.TreeIs.sendingValueBinding tr = do
    fishSendEnv <- fishSend env tr
    procExecute fishSendEnv trs
  | Tree.nodeStrictlySatisfies ((B.Send ==) . SyntaxUnit.context) tr =
    execute env tr >> procExecute env trs
  | Tree.nodeStrictlySatisfies ((B.Return ==) . SyntaxUnit.context) tr =
    execute env tr
  | otherwise = return D.Null

execute :: EnvironmentStack -> SyntaxTree -> IO D.Data
execute env tr
  | Tree.nodeStrictlySatisfies Check.NodeIs.dataTokenAndPrimitive tr =
    evaluatePrimitiveData tr
  | Tree.nodeStrictlySatisfies Check.NodeIs.fin tr = evaluateFin env tr
  | Tree.nodeStrictlySatisfies Check.NodeIs.operator tr = evaluateOperator env tr
  | Check.TreeIs.standardLibCall tr =
    case ( D.fromData
             . DMaybe.fromJust
             . SakanaParser.baseData
             . SyntaxUnit.token
             . DMaybe.fromJust
             . Tree.treeNode
         )
      tr of
      "trout" ->
        trout
          env
          ((DMaybe.fromJust . Util.General.head' . Tree.treeChildren) tr)
      "herring" ->
        herring
          env
          ((DMaybe.fromJust . Util.General.head' . Tree.treeChildren) tr)
      "dolphin" -> dolphin
      "read" ->
        sakanaRead
          env
          ((DMaybe.fromJust . Util.General.head' . Tree.treeChildren) tr)
      "floor" ->
        sakanaFloor env ((DMaybe.fromJust . Util.General.head' . Tree.treeChildren) tr)
      _ -> return D.Null
  | Check.TreeIs.functionCall tr =
    executeFunctionCall env tr
  | otherwise = return D.Null

evaluatePrimitiveData :: SyntaxTree -> IO D.Data
evaluatePrimitiveData = return . getNodeTokenBaseData

evaluateFin :: EnvironmentStack -> SyntaxTree -> IO D.Data
evaluateFin env tr = fin env (fstArg tr) (sndArg tr) (thdArg tr)
  where
    fromMaybeFinArg tr' xs =
      DMaybe.fromMaybe
        ((missingPositionalArgumentException . Tree.treeChildren) tr')
        (Util.General.head' xs)
    fstArg tr' = fromMaybeFinArg tr' (Tree.treeChildren tr')
    sndArg tr' = fromMaybeFinArg tr' ((Util.General.tail' . Tree.treeChildren) tr')
    thdArg tr' =
      fromMaybeFinArg
        tr'
        ((Util.General.tail' . Util.General.tail' . Tree.treeChildren) tr')

evaluateOperator :: EnvironmentStack -> SyntaxTree -> IO D.Data
evaluateOperator env tr = do
  let ioOperatorArguments = getOperatorArgs env tr
  argx <- fst ioOperatorArguments
  argy <- snd ioOperatorArguments
  if Util.General.both D.isNumeric (argx, argy)
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
      if Util.General.both (D.String "" `LikeClass.like`) (argx, argy)
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
          if Util.General.both (D.Boolean True `LikeClass.like`) (argx, argy)
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
    getNodeOperator tr' = case getNodeToken tr' of (SakanaParser.Operator o) -> o; _ -> O.Eq
    justUnNum = DMaybe.fromJust . D.unNum
    justUnString = DMaybe.fromJust . D.unString
    justUnBoolean = DMaybe.fromJust . D.unBoolean
    undefinedOperatorBehaviorException opString argStrAndType =
      Exception.raiseError $
        Exception.newException
          Exception.UndefinedOperatorBehavior
          [SakanaParser.getSyntaxAttributeFromTree SyntaxUnit.line tr]
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
          [SakanaParser.getSyntaxAttributeFromTree SyntaxUnit.line tr]
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

calledFunctionSymbol :: EnvironmentStack -> SakanaParser.SyntaxTree -> SymbolPair
calledFunctionSymbol env =
  lookupSymbolInEnvironmentStack env . DMaybe.fromJust . Tree.treeNode

----get information from a tree-----------------------------------------------------------
------------------------------------------------------------------------------------------

getNodeTokenBaseData :: SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (DMaybe.fromMaybe D.Null . (SakanaParser.baseData . SyntaxUnit.token))

getNodeToken :: SyntaxTree -> SakanaParser.Token
getNodeToken = Tree.maybeOnTreeNode (SakanaParser.Data D.Null) SyntaxUnit.token

getOperatorArgs :: EnvironmentStack -> SyntaxTree -> (IO D.Data, IO D.Data)
getOperatorArgs env tr =
  ( (execute env . DMaybe.fromJust . Util.General.head' . Tree.treeChildren) tr,
    ( execute env
        . DMaybe.fromJust
        . Util.General.head'
        . Util.General.tail'
        . Tree.treeChildren
    )
      tr
  )

getFuncDeclArgs :: SyntaxTree -> [SyntaxTree]
getFuncDeclArgs =
  filter (Tree.nodeStrictlySatisfies (not . Check.NodeIs.nullNode))
    . filter (Tree.nodeStrictlySatisfies Check.NodeIs.sendContext)
    . Util.General.tail'
    . Tree.treeChildren

getFunctionDeclPositionalArgs :: SyntaxTree -> [SyntaxTree]
getFunctionDeclPositionalArgs =
  filter (Check.TreeIs.positionalArg) . getFuncDeclArgs

----misc----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

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

-- This function is required to get all of the execution trees of a function
-- declaration, which is why 'getMainExecutionTrees' is called.
-- Ideally, a function declaration works like a miniature program.
-- So it is only natural that it's execution trees are denoted by the 'swim' keyword.
getExecutableChildren :: SyntaxTree -> [SyntaxTree]
getExecutableChildren tr = getMainExecutionTrees tr

-- This function is required to get and potentially bind all provided bindable
-- information in a function declaration, like extra sub-function declarations.
getBindableArgs :: SyntaxTree -> [SyntaxTree]
getBindableArgs = DList.takeWhile (not . Check.TreeIs.swim) . getFuncDeclArgs

makeSymbolTableFromFuncCall ::
  EnvironmentStack -> SymbolTable -> [SyntaxTree] -> [SyntaxTree] -> IO SymbolTable
makeSymbolTableFromFuncCall _ table _ [] = return table
makeSymbolTableFromFuncCall _ table [] dfargs =
  if any Check.TreeIs.positionalArg dfargs
    then missingPositionalArgumentException dfargs
    else
      ( return
          . flip (++) table
          . map DMaybe.fromJust
          . filter (not . DMaybe.isNothing)
          . map (maybeTreeToSymbolPair table)
      )
        dfargs
makeSymbolTableFromFuncCall mainExEnv table (cfarg : cfargs) (dfarg : dfargs)
  | Check.TreeIs.positionalArg dfarg = do
    -- cfargVal <- execute mainExEnv cfarg
    -- argValBinding <- return $ createSymbolPairFromArgTreePair dfarg cfargVal
    --It is necessary to store an argument symbol as a tree so that functions can
    -- be passed as arguments into other functions.
    -- It allows the use of higher-order functions.
    argBinding <- (return . createSymbolPairTreeFromArgTreePair dfarg) cfarg
    makeSymbolTableFromFuncCall mainExEnv (argBinding : table) cfargs dfargs
  | otherwise = do
    let fromJustSymbolTable =
          DMaybe.maybe table (: table) (maybeTreeToSymbolPair table dfarg)
    makeSymbolTableFromFuncCall mainExEnv (fromJustSymbolTable) cfargs dfargs

missingPositionalArgumentException :: [SakanaParser.SyntaxTree] -> a2
missingPositionalArgumentException fdas =
  Exception.raiseError $
    Exception.newException
      Exception.MissingPositionalArguments
      (map (Tree.maybeOnTreeNode 0 SyntaxUnit.line) fdas)
      ( "Missing positional arguments:\n"
          ++ (unlines . map (Tree.maybeOnTreeNode "N/A" (show))) fdas
      )
      Exception.Fatal

createSymbolPairTreeFromArgTreePair ::
  SakanaParser.SyntaxTree ->
  SakanaParser.SyntaxTree ->
  Env.SymbolPair
createSymbolPairTreeFromArgTreePair dfarg =
  makeSymbolPair
    . (Tree.-<-) dfarg
    . flip Tree.mutateTreeNode (SakanaParser.setContext B.Return)

createSymbolPairFromArgTreePair :: SyntaxTree -> D.Data -> SymbolPair
createSymbolPairFromArgTreePair dfarg' cfargVal' =
  makeSymbolPair $
    (Tree.tree . DMaybe.fromJust . Tree.treeNode) dfarg'
      Tree.-<- ( Tree.tree
                   . SakanaParser.setContext B.Return
                   . SakanaParser.genericSyntaxUnit
                   . SakanaParser.Data
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

fin :: EnvironmentStack -> SyntaxTree -> SyntaxTree -> SyntaxTree -> IO D.Data
fin env cond forTrue forFalse = do
  condValue <- (procExecute env . getExecutableChildrenOrNode) cond
  if truthy condValue
    then (procExecute env . getExecutableChildrenOrNode) forTrue
    else (procExecute env . getExecutableChildrenOrNode) forFalse
  where
    getExecutableChildrenOrNode :: SyntaxTree -> [SyntaxTree]
    getExecutableChildrenOrNode tr =
      if Check.TreeIs.swim tr
        then Tree.treeChildren tr
        else [recontextualizeFinChild tr]
    -- This function is required because fin's arguments have a send context,
    -- but if a value is required after procExecution, they must have a return type,
    -- or a Null value will be
    -- returned.
    recontextualizeFinChild :: SyntaxTree -> SyntaxTree
    recontextualizeFinChild = flip Tree.mutateTreeNode (SakanaParser.setContext B.Return)

trout :: EnvironmentStack -> SyntaxTree -> IO D.Data
trout env toPrint =
  (execute env toPrint >>= (hPutStr stdout . D.fromData)) >> return D.Null

herring :: EnvironmentStack -> SyntaxTree -> IO D.Data
herring env toPrint =
  (execute env toPrint >>= (hPutStr stderr . D.fromData)) >> return D.Null

dolphin :: IO D.Data
dolphin = hGetLine stdin >>= return . D.String

fishSend :: EnvironmentStack -> SyntaxTree -> IO EnvironmentStack
fishSend env encTree = do
  let encrustedSymbolDataIO =
        (execute env . DMaybe.fromJust . Util.General.head' . Tree.treeChildren) encTree
  encrustedSymbolData <- encrustedSymbolDataIO
  encrustedSymbolPair <-
    return $
      createSymbolPairFromArgTreePair (encTree) encrustedSymbolData
  return ([encrustedSymbolPair] : env)

sakanaRead :: Env.EnvironmentStack -> SakanaParser.SyntaxTree -> IO D.Data
sakanaRead env tr = do
  valueResult <- execute env tr
  let unstringedValue = D.unString valueResult
  DMaybe.maybe
    (sakanaReadException valueResult "Value is not a string.")
    (return . readSakanaData)
    (unstringedValue)
  where
    readSakanaData d =
      if D.isPrimitive maybePrimData
        then maybePrimData
        else
          sakanaReadException
            d
            ( "Value is no the correct format of a Sakana Primitive."
                ++ "\n\tMust be a double, string or boolean."
            )
      where
        maybePrimData = D.readData d
    sakanaReadException d supplementalMessage =
      Exception.raiseError $
        Exception.newException
          Exception.GeneralTypeError
          []
          ("Error reading string: " ++ show d ++ "\n\t" ++ supplementalMessage)
          Exception.Fatal

sakanaFloor :: Env.EnvironmentStack -> SakanaParser.SyntaxTree -> IO D.Data
sakanaFloor env tr = do
  valueResult <- execute env tr
  let maybeNum = D.unNum valueResult
  DMaybe.maybe
    (sakanaFloorError valueResult)
    ((return . D.Num . fromIntegral . floor))
    (maybeNum)
  where
    sakanaFloorError n =
      Exception.raiseError $
        Exception.newException
          Exception.GeneralTypeError
          []
          ("Error calculating floor, not a Num" ++ show n)
          Exception.Fatal

----testing-------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

s' :: [Char]
s' =
  "fish apply >(f)> >(x)> <(f >(x)>)<\
  \ fish add >(x)> <( >(y)> <(+ >(x)> >(y)>)<)< <(apply >(add >(1)>)> >(1)>)<"

pt' = SakanaParser.generateSyntaxTree s'

e' = getMainEnvironmentStack pt'

et' = getMainExecutionTrees pt'

ex' = executeMain (return e') (return et') (return "")
