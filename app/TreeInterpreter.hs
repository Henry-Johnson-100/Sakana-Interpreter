module TreeInterpreter
  ( executeMain,
    execute,
    getMainExecutionTrees,
    getMainRuntime,
    getMainEnvironmentStack,
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

import qualified Data.Char as DChar
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DList
import qualified Data.Maybe as DMaybe
import qualified Data.Tuple as DTuple
import qualified Exception.Base as Exception
import qualified SakanaParser
import qualified SakanaParser as SyntaxUnit (SyntaxUnit (context, line, token))
import qualified System.Environment
import qualified System.IO
import qualified Token.Bracket as B
import qualified Token.Control as C
import qualified Token.Data as D
import qualified Token.Keyword as K
import qualified Token.Operator as O
import qualified TreeInterpreter.Environment as Env
import qualified TreeInterpreter.LocalCheck.NodeIs as Check.NodeIs
import qualified TreeInterpreter.LocalCheck.TreeIs as Check.TreeIs
import qualified Util.General
import qualified Util.Like as LikeClass
import qualified Util.Tree as Tree

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

--Evaluation functions used to take a tree and return some FISH value.--------------------
------------------------------------------------------------------------------------------
getMainEnvironmentStack ::
  SakanaParser.SyntaxTree -> Env.SakanaRuntime SakanaParser.SyntaxTree
getMainEnvironmentStack = Env.makeNewRuntime . Tree.treeChildren

getMainExecutionTrees ::
  SakanaParser.SyntaxTree -> Env.SakanaRuntime [SakanaParser.SyntaxTree]
getMainExecutionTrees docTree =
  DMaybe.maybe
    (Env.runtimeUnit [getLastExecutionTree docTree])
    (Env.runtimeUnit . Tree.treeChildren)
    ((DList.find Check.TreeIs.swim . Tree.treeChildren) docTree)
  where
    getLastExecutionTree =
      DMaybe.fromMaybe Tree.Empty
        . Util.General.last'
        . filter (Check.TreeIs.executable)
        . Tree.treeChildren

getMainRuntime ::
  SakanaParser.SyntaxTree ->
  IO (Env.SakanaRuntime [SakanaParser.SyntaxTree])
-- Env.SakanaRuntimeT IO [SakanaParser.SyntaxTree]
getMainRuntime trs = do
  let mainSymbolEnvSR = getMainEnvironmentStack trs
      mainExTreesSR = getMainExecutionTrees trs
  return (Env.SakanaRuntime (Env.sakanaEnv mainSymbolEnvSR) (Env.sakanaVal mainExTreesSR))

-- | This will be our new main entry point for a fish program
-- If no execution tree can be found in 'main' then a single tree containing a null value
-- will be returned.
executeMain ::
  IO (Env.SakanaRuntime [SakanaParser.SyntaxTree]) ->
  IO String ->
  IO D.Data
executeMain srt programArgsIO = do
  programArgs <- programArgsIO
  programRuntime <- srt
  let programRuntimeWithArgs = bindSakanaArgsToArgs programRuntime programArgs
  procExecute programRuntimeWithArgs
  where
    bindSakanaArgsToArgs ::
      Env.SakanaRuntime [SakanaParser.SyntaxTree] ->
      String ->
      Env.SakanaRuntime [SakanaParser.SyntaxTree]
    bindSakanaArgsToArgs sr args =
      ( Env.addSymbolToRuntime sr . Env.makeSymbolPair
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

-- | Will cease execution and return at the first return context it sees
procExecute :: Env.SakanaRuntime [SakanaParser.SyntaxTree] -> IO D.Data
procExecute (Env.SakanaRuntime _ []) = return D.Null
procExecute (Env.SakanaRuntime env (tr : trs))
  | Check.TreeIs.sendingValueBinding tr = do
    fishSendRuntime <- fishSend (Env.SakanaRuntime env tr)
    let newUnionedEnv = HashMap.union (Env.sakanaEnv fishSendRuntime) env
    (procExecute . Env.SakanaRuntime newUnionedEnv) trs
  | Tree.nodeStrictlySatisfies ((B.Send ==) . SyntaxUnit.context) tr =
    execute srtr >> procExecute srtrs
  | Tree.nodeStrictlySatisfies ((B.Return ==) . SyntaxUnit.context) tr =
    execute srtr
  | otherwise = return D.Null
  where
    trx = tr : trs
    srt = Env.SakanaRuntime env trx
    srtr = Env.SakanaRuntime env tr
    srtrs = Env.SakanaRuntime env trs

execute :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
execute sr
  | (Tree.nodeStrictlySatisfies Check.NodeIs.dataTokenAndPrimitive . Env.sakanaVal) sr =
    evaluatePrimitiveData sr
  | (Tree.nodeStrictlySatisfies Check.NodeIs.fin . Env.sakanaVal) sr = evaluateFin sr
  | (Tree.nodeStrictlySatisfies Check.NodeIs.operator . Env.sakanaVal) sr =
    evaluateOperator sr
  | (Check.TreeIs.standardLibCall . Env.sakanaVal) sr =
    case ( D.fromData
             . DMaybe.fromJust
             . SakanaParser.baseData
             . SyntaxUnit.token
             . DMaybe.fromJust
             . Tree.treeNode
             . Env.sakanaVal
         )
      sr of
      "trout" -> trout sr {Env.sakanaVal = getStdLibArg sr}
      "herring" -> herring sr {Env.sakanaVal = getStdLibArg sr}
      "dolphin" -> dolphin
      "read" -> sakanaRead sr {Env.sakanaVal = getStdLibArg sr}
      "floor" -> sakanaFloor sr {Env.sakanaVal = getStdLibArg sr}
      _ -> return D.Null
  | (Check.TreeIs.functionCall . Env.sakanaVal) sr =
    executeFunctionCall sr
  | otherwise = return D.Null
  where
    getStdLibArg =
      DMaybe.fromJust . Util.General.head' . Tree.treeChildren . Env.sakanaVal

evaluatePrimitiveData :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
evaluatePrimitiveData = return . getNodeTokenBaseData . Env.sakanaVal

evaluateFin :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
evaluateFin (Env.SakanaRuntime env tr) =
  (fin . Env.SakanaRuntime env) (fstArg tr, sndArg tr, thdArg tr)
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

evaluateOperator :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
evaluateOperator (Env.SakanaRuntime env tr) = do
  let ioOperatorArguments = getOperatorArgs (Env.SakanaRuntime env tr)
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
    getNodeOperator tr' =
      case getNodeToken tr' of (SakanaParser.Operator o) -> o; _ -> O.Eq
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

executeFunctionCall :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
executeFunctionCall sr = do
  let newRuntimeT = newClosureRuntime sr functionDeclaration
  newRuntime <- newRuntimeT
  procExecute newRuntime
  where
    functionDeclaration =
      Env.symbolVal
        (Env.lookupSymbol (sr) ((DMaybe.fromJust . Tree.treeNode . Env.sakanaVal) sr))

----get information from a tree-----------------------------------------------------------
------------------------------------------------------------------------------------------

getNodeTokenBaseData :: SakanaParser.SyntaxTree -> D.Data
getNodeTokenBaseData =
  Tree.maybeOnTreeNode
    D.Null
    (DMaybe.fromMaybe D.Null . (SakanaParser.baseData . SyntaxUnit.token))

getNodeToken :: SakanaParser.SyntaxTree -> SakanaParser.Token
getNodeToken = Tree.maybeOnTreeNode (SakanaParser.Data D.Null) SyntaxUnit.token

-- Just LOL
getOperatorArgs :: Env.SakanaRuntime SakanaParser.SyntaxTree -> (IO D.Data, IO D.Data)
getOperatorArgs sr = do
  let fstArgRuntime =
        sr
          { Env.sakanaVal =
              ( DMaybe.fromJust
                  . Util.General.head'
                  . Tree.treeChildren
                  . Env.sakanaVal
              )
                sr
          }
      sndArgRuntime =
        sr
          { Env.sakanaVal =
              ( DMaybe.fromJust
                  . Util.General.head'
                  . Util.General.tail'
                  . Tree.treeChildren
                  . Env.sakanaVal
              )
                sr
          }
      argResults = (execute fstArgRuntime, execute sndArgRuntime)
  (argResults)

getFuncDeclArgs :: SakanaParser.SyntaxTree -> [SakanaParser.SyntaxTree]
getFuncDeclArgs =
  filter (Tree.nodeStrictlySatisfies (not . Check.NodeIs.nullNode))
    . Util.General.tail'
    . Util.General.init'
    . Tree.treeChildren

getFunctionDeclPositionalArgs :: SakanaParser.SyntaxTree -> [SakanaParser.SyntaxTree]
getFunctionDeclPositionalArgs =
  filter (Check.TreeIs.positionalArg) . getFuncDeclArgs

----misc----------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | This function serves to create a new runtime environment
newClosureRuntime ::
  Env.SakanaRuntime SakanaParser.SyntaxTree ->
  SakanaParser.SyntaxTree ->
  IO (Env.SakanaRuntime [SakanaParser.SyntaxTree])
newClosureRuntime sr functionDeclaration = do
  let newRuntimeEnvIO =
        makeIOEnvFromFuncCall
          (Env.SakanaRuntime (Env.sakanaEnv sr) ((Tree.treeChildren . Env.sakanaVal) sr))
          (getBindableArgs functionDeclaration)
      newRuntimeVal = getMainExecutionTrees functionDeclaration
      newWholeRuntime = do
        nre <- newRuntimeEnvIO
        return newRuntimeVal {Env.sakanaEnv = Env.sakanaEnv nre}
  newWholeRuntime

-- This function is required to get and potentially bind all provided bindable
-- information in a function declaration, like extra sub-function declarations.
getBindableArgs :: SakanaParser.SyntaxTree -> [SakanaParser.SyntaxTree]
getBindableArgs = DList.takeWhile (not . Check.TreeIs.swim) . getFuncDeclArgs

-- | By far the messiest function in this module, I definitely want to put in the effort
-- to refactor this one to make it actually legible.
makeSymbolTableFromFuncCall ::
  Env.SakanaRuntime [SakanaParser.SyntaxTree] ->
  Env.SymbolTable ->
  [SakanaParser.SyntaxTree] ->
  IO (Env.SakanaRuntime [SakanaParser.SyntaxTree])
makeSymbolTableFromFuncCall _ table [] =
  (return (Env.SakanaRuntime table []))
makeSymbolTableFromFuncCall (Env.SakanaRuntime mainExEnv []) table dfargs =
  if any Check.TreeIs.positionalArg dfargs
    then missingPositionalArgumentException dfargs
    else do
      let remainingNonPosArgTable =
            ( DList.foldr Env.addSymbolPairToTable table
                . map DMaybe.fromJust
                . filter DMaybe.isJust
                . map (flip Env.treeToSymbolPairIfNotAssigned table)
            )
              dfargs
          globalUnionToNewRuntime =
            HashMap.union (HashMap.union remainingNonPosArgTable table) mainExEnv
      (return (Env.SakanaRuntime globalUnionToNewRuntime []))
makeSymbolTableFromFuncCall
  (Env.SakanaRuntime mainExEnv (cfarg : cfargs))
  table
  (dfarg : dfargs)
    | Check.TreeIs.positionalArg dfarg = do
      let cfargValIO = execute (Env.SakanaRuntime mainExEnv cfarg)
      cfargVal <- cfargValIO
      let argValBinding =
            createSymbolPairFromArgTreePair dfarg cfargVal
      makeSymbolTableFromFuncCall
        (Env.SakanaRuntime mainExEnv cfargs)
        (Env.addSymbolPairToTable argValBinding table)
        dfargs
    | otherwise = do
      let fromJustSymbolTable =
            DMaybe.maybe
              table
              (flip Env.addSymbolPairToTable table)
              (Env.treeToSymbolPairIfNotAssigned dfarg table)
      makeSymbolTableFromFuncCall
        (Env.SakanaRuntime mainExEnv cfargs)
        (fromJustSymbolTable)
        dfargs

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

-- | From the passed syntax tree, create a symbol pair of the value of the passed data.
createSymbolPairFromArgTreePair :: SakanaParser.SyntaxTree -> D.Data -> Env.SymbolPair
createSymbolPairFromArgTreePair dfarg' cfargVal' =
  Env.makeSymbolPair $
    (Tree.tree . DMaybe.fromJust . Tree.treeNode) dfarg'
      Tree.-<- ( Tree.tree
                   . SakanaParser.setContext B.Return
                   . SakanaParser.genericSyntaxUnit
                   . SakanaParser.Data
               )
        cfargVal'

makeIOEnvFromFuncCall ::
  Env.SakanaRuntime [SakanaParser.SyntaxTree] ->
  [SakanaParser.SyntaxTree] ->
  IO (Env.SakanaRuntime [SakanaParser.SyntaxTree])
makeIOEnvFromFuncCall runtimeAtCall declarationArgs = do
  let newSubScopeIO =
        makeSymbolTableFromFuncCall runtimeAtCall HashMap.empty declarationArgs
  newSubScope <- newSubScopeIO
  return
    runtimeAtCall
      { Env.sakanaEnv =
          HashMap.union (Env.sakanaEnv newSubScope) (Env.sakanaEnv runtimeAtCall)
      }

----Standard Library Functions------------------------------------------------------------
------------------------------------------------------------------------------------------
sakanaStandardLibrary :: [String]
sakanaStandardLibrary = ["trout", "dolphin"]

sakanaPrint :: D.Data -> IO ()
sakanaPrint = System.IO.hPutStrLn System.IO.stdout . D.fromData

fin ::
  Env.SakanaRuntime
    ( SakanaParser.SyntaxTree,
      SakanaParser.SyntaxTree,
      SakanaParser.SyntaxTree
    ) ->
  IO D.Data
fin srt3 = do
  condValue <-
    procExecute
      srt3
        { Env.sakanaVal = (getExecutableChildrenOrNode . cond) srt3 -- Lord have mercy ☦︎
        }
  if truthy condValue
    then procExecute srt3 {Env.sakanaVal = (getExecutableChildrenOrNode . forTrue) srt3}
    else procExecute srt3 {Env.sakanaVal = (getExecutableChildrenOrNode . forFalse) srt3}
  where
    getExecutableChildrenOrNode :: SakanaParser.SyntaxTree -> [SakanaParser.SyntaxTree]
    getExecutableChildrenOrNode tr =
      if Check.TreeIs.swim tr
        then Tree.treeChildren tr
        else [recontextualizeFinChild tr]
    -- This function is required because fin's arguments have a send context,
    -- but if a value is required after procExecution, they must have a return type,
    -- or a Null value will be
    -- returned.
    recontextualizeFinChild :: SakanaParser.SyntaxTree -> SakanaParser.SyntaxTree
    recontextualizeFinChild = flip Tree.mutateTreeNode (SakanaParser.setContext B.Return)
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x
    snd3 :: (a, b, c) -> b
    snd3 (_, x, _) = x
    thd3 :: (a, b, c) -> c
    thd3 (_, _, x) = x
    cond :: Env.SakanaRuntime (c1, b, c2) -> c1
    cond = fst3 . Env.sakanaVal
    forTrue :: Env.SakanaRuntime (a, c1, c2) -> c1
    forTrue = snd3 . Env.sakanaVal
    forFalse :: Env.SakanaRuntime (a, b, c) -> c
    forFalse = thd3 . Env.sakanaVal

trout :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
trout srt =
  (execute srt >>= (System.IO.hPutStr System.IO.stdout . D.fromData)) >> return D.Null

herring :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
herring srt =
  (execute srt >>= (System.IO.hPutStr System.IO.stderr . D.fromData)) >> return D.Null

dolphin :: IO D.Data
dolphin = System.IO.hGetLine System.IO.stdin >>= return . D.String

fishSend ::
  Env.SakanaRuntime SakanaParser.SyntaxTree ->
  IO (Env.SakanaRuntime [SakanaParser.SyntaxTree])
fishSend srt = do
  let expressionRuntimeToSend =
        srt
          { Env.sakanaVal =
              ( DMaybe.fromJust
                  . Util.General.head'
                  . Tree.treeChildren
                  . Env.sakanaVal
              )
                srt
          }
      symbolValueToSendIO = execute expressionRuntimeToSend
  symbolValueToSend <- symbolValueToSendIO
  let newSymbolPair =
        createSymbolPairFromArgTreePair (Env.sakanaVal srt) (symbolValueToSend)
      newRuntime = Env.addSymbolToRuntime srt newSymbolPair
  return (newRuntime {Env.sakanaVal = [Env.sakanaVal newRuntime]})

sakanaRead :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
sakanaRead srt = do
  valueResult <- execute srt
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

sakanaFloor :: Env.SakanaRuntime SakanaParser.SyntaxTree -> IO D.Data
sakanaFloor srt = do
  valueResult <- execute srt
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
  "fish thing >()> swim >(trout >(\"Hello\nthere\")>)> <(0)< swim <(thing)<"

pt' = SakanaParser.generateSyntaxTree s'

e' = getMainEnvironmentStack pt'

et' = getMainExecutionTrees pt'

sr' = getMainRuntime pt'

ex' = executeMain sr' (return "")
