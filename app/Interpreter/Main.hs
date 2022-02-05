module Interpreter.Main
  ( createCLIArgumentBindings,
    evaluateProgram,
  )
where

import Control.Monad ((<=<))
import qualified Control.Monad as CMonad
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List hiding (lookup)
import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified Interpreter.Environment as Env
import qualified Interpreter.Inspection as Inspect
import qualified Interpreter.SknStdLib.Std as SknStdLib
import qualified Parser.Main
import qualified Parser.Syntax as Syntax
import qualified System.IO as IO
import qualified Util.Classes as UC
import Util.General ((.<))
import qualified Util.General as UGen
import Util.Tree (Tree ((:-<-:)))
import qualified Util.Tree as Tree
import Prelude hiding (lookup)

----Interpreter Preprocessing and Output--------------------------------------------------
------------------------------------------------------------------------------------------

-- | ...
evaluateProgram ::
  Syntax.SyntaxTree -> [Env.Binding] -> IO (Either.Either Syntax.SyntaxTree Syntax.Data)
evaluateProgram docTree additionalBindings =
  ( fmap getUsefulOutput
      . fmap programOutputHead
      . interpret
      . preprocessParserOutput docTree
  )
    additionalBindings
  where
    programOutputHead :: Env.Runtime -> Syntax.SyntaxTree
    programOutputHead =
      Maybe.fromMaybe
        (Exception.raiseError programTerminatesAsNull)
        . (UGen.head' . Env.runtimeValue . Env.throwJustError)
    programTerminatesAsNull :: Exception.Exception
    programTerminatesAsNull =
      Exception.newException
        Exception.NullTree
        []
        "Null value at end of program."
        Exception.Fatal
    -- If the output of the interpreter's head is a primitive data type,
    -- return that instead otherwise return the tree that was the result.
    getUsefulOutput :: Syntax.SyntaxTree -> Either.Either Syntax.SyntaxTree Syntax.Data
    getUsefulOutput tr =
      if Inspect.treeHeadIsPrimitiveData tr
        then
          ( Either.Right
              . Maybe.fromJust
              . (=<<) (Syntax.baseData . Syntax.token)
              . Tree.treeNode
          )
            tr
        else Either.Left tr
    -- Takes the raw output from the parser and converts it into an interpretable Runtime.
    preprocessParserOutput :: Syntax.SyntaxTree -> [Env.Binding] -> Env.Runtime
    preprocessParserOutput docTree additionalBindings =
      ( setMain
          . injectAdditionalBindings additionalBindings
          . foldStatementTreesToRuntime
      )
        UC.defaultValue {Env.runtimeValue = Tree.treeChildren docTree}
      where
        -- Finds the function identified as 'main' in the runtime's symbol table
        -- and returns a runtime with that function's value as the runtimeValue.
        setMain :: Env.Runtime -> Env.Runtime
        setMain rt =
          Maybe.maybe
            (Env.replaceException (Maybe.Just noMainException) rt)
            (flip Env.replaceValue rt . UGen.listSingleton . getLampreyValue)
            (Env.runtimeMaybeLookup rt "main")
          where
            noMainException =
              Exception.newException
                Exception.SymbolNotFound
                []
                "requires a fish called \'main\' to interpret the program."
                Exception.Fatal
        injectAdditionalBindings :: [Env.Binding] -> Env.Runtime -> Env.Runtime
        injectAdditionalBindings bindings rt = List.foldr Env.injectBinding rt bindings

foldStatementTreesToRuntime :: Env.Runtime -> Env.Runtime
foldStatementTreesToRuntime rt =
  if (null . Env.runtimeValue) rt
    then rt
    else
      ( foldStatementTreesToRuntime
          . advanceToNextTree
          . processStatementTree
      )
        rt

createCLIArgumentBindings :: [String] -> [Env.Binding]
createCLIArgumentBindings = map bindingFromTuple . zip argNameScheme
  where
    argNameScheme :: [String]
    argNameScheme = ["_arg_" ++ [c] | c <- ['A' .. 'Z'] ++ ['a' .. 'z']]
    bindingFromTuple :: (String, String) -> Env.Binding
    bindingFromTuple (bid, bval) =
      Env.Binding
        (Env.BindingKey bid)
        []
        (Tree.tree (UC.defaultValue {Syntax.token = Syntax.Data (Syntax.String bval)}))

----Interpreter Logic and Operations------------------------------------------------------
------------------------------------------------------------------------------------------

interpret :: Env.Runtime -> IO Env.Runtime
interpret rt = interpret' . Env.throwJustError $ rt
  where
    interpret' :: Env.Runtime -> IO Env.Runtime
    -- Will cause an infinite hang if Env.throwJustError is not present in interpret.
    interpret' (Env.Runtime st [] err) =
      interpret
        ( Env.replaceException
            ( Maybe.Just
                ( Exception.newException
                    Exception.NullTree
                    []
                    "Unexpected null tree in the interpreter."
                    Exception.Fatal
                )
            )
            rt
        )
    -- Guards go here.
    -- trs could be null.
    interpret' (Env.Runtime st (tr : trs) err)
      | Inspect.treeHeadIsPrimitiveData tr = return rt
      | Inspect.treeHeadIsFunctionCall tr = evaluateFunction rt
      | otherwise = return rt

----Function Interpretation---------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | #TODO
evaluateFunction :: Env.Runtime -> IO Env.Runtime
evaluateFunction rt = evaluateFunction' rt
  where
    evaluateFunction' :: Env.Runtime -> IO Env.Runtime
    evaluateFunction' (Env.Runtime st (tr : trs) err)
      | Tree.nodeStrictlySatisfies nodeIsStandardLibCall tr =
        evaluateStandardLibraryCall rt
      | otherwise = return rt

-- | Responsible for recieving the calling runtime environment and the binding which
-- holds the function definition then creating a new runtime with the definition's
-- parameters bound to the caller's arguments.
--
-- Should be able to handle returning curried functions or whatever incomplete
-- arg lists may entail in the future.
--
-- Also needs to bind function and struct definitions that are a part of the
-- function's declaration.
createFunctionArgumentBindings :: Env.Runtime -> Env.Binding -> Env.Runtime
createFunctionArgumentBindings (Env.Runtime st (call : trs) err) def =
  let callerArgs =
        ( filter (Tree.nodeStrictlySatisfies Inspect.nodeIsSendContext)
            . Tree.treeChildren
        )
          call
      partitionedDefParameters =
        ( partitionDefParameters
            . filter (Tree.nodeStrictlySatisfies Inspect.nodeIsSendContext)
            . Tree.treeChildren
            . Env.bindingTree
        )
          def
      boundArgumentRuntime =
        bindArgumentsToPositionalParameters
          (Env.replaceValue callerArgs UC.defaultValue)
          ((Maybe.mapMaybe Tree.treeNode . fst) partitionedDefParameters)
      boundNonPosParamRuntime =
        (foldNonPositionalParamsToRuntime . snd) partitionedDefParameters
      newInterpreterRuntime = Env.Runtime st trs err
      propagatedException =
        ( Env.runtimeException
            . List.foldl' Env.propagateException UC.defaultValue
        )
          [boundArgumentRuntime, boundNonPosParamRuntime, newInterpreterRuntime]
   in ( Env.replaceException propagatedException
          . List.foldl' Env.transUnion UC.defaultValue
      )
        [boundNonPosParamRuntime, boundArgumentRuntime, newInterpreterRuntime]
  where
    bindArgumentsToPositionalParameters ::
      Env.Runtime -> [Syntax.SyntaxUnit] -> Env.Runtime
    bindArgumentsToPositionalParameters (Env.Runtime sts args err) [] =
      -- Too many arguments provided
      (Env.Runtime sts args (Maybe.Just generalException))
    bindArgumentsToPositionalParameters
      callerArgRuntime
      (param : params) =
        case callerArgRuntime of
          (Env.Runtime sts [] err) ->
            -- done binding, return the runtime
            callerArgRuntime
          (Env.Runtime (st : sts) (arg : args) err) ->
            bindArgumentsToPositionalParameters
              ( Env.Runtime
                  (((Env.insertBinding st .< bindArgument) param arg) : sts)
                  args
                  err
              )
              params
        where
          bindArgument :: Syntax.SyntaxUnit -> Syntax.SyntaxTree -> Env.Binding
          bindArgument param' arg' = Env.Binding (getParamBindingKey param') [] arg'
          getParamBindingKey :: Syntax.SyntaxUnit -> Env.BindingKey
          getParamBindingKey =
            Env.BindingKey
              . Maybe.fromJust
              . (=<<) Syntax.unId
              . Syntax.baseData
              . Syntax.token
    partitionDefParameters ::
      [Syntax.SyntaxTree] -> ([Syntax.SyntaxTree], [Syntax.SyntaxTree])
    partitionDefParameters = List.partition (Inspect.treeHeadIsPositionalParameter)
    foldNonPositionalParamsToRuntime :: [Syntax.SyntaxTree] -> Env.Runtime
    foldNonPositionalParamsToRuntime trs =
      foldStatementTreesToRuntime
        (Env.Runtime [] trs Maybe.Nothing)

-- | #TODO
-- still requires implementing argument number error checking and stuff
-- but I think it works for now
evaluateStandardLibraryCall :: Env.Runtime -> IO Env.Runtime
evaluateStandardLibraryCall rt = evaluateStandardLibraryCall' rt
  where
    evaluateStandardLibraryCall' :: Env.Runtime -> IO Env.Runtime
    evaluateStandardLibraryCall' (Env.Runtime st (tr : trs) err) = do
      evaluatedArguments <-
        interpretArgumentList
          (Env.replaceValue (Tree.treeChildren tr) rt)
      stdLibFuncResult <-
        ( SknStdLib.stdLibFunctionDefinition
            ((getStdLibFunctionFromId . getBaseFunctionIdString) tr)
          )
          evaluatedArguments
      (return . flip Env.replaceValue rt . UGen.listSingleton) stdLibFuncResult
      where
        getBaseFunctionIdString :: Syntax.SyntaxTree -> String
        getBaseFunctionIdString =
          ( Maybe.fromJust
              . (=<<) Syntax.unId
              . (=<<) (Syntax.baseData . Syntax.token)
              . Tree.treeNode
          )
        getStdLibFunctionFromId :: String -> SknStdLib.SknStdLibFunction
        getStdLibFunctionFromId funcId =
          ( Maybe.fromJust
              . UGen.head'
              . filter ((==) funcId . SknStdLib.stdLibFunctionId)
          )
            SknStdLib.exporting
        -- Taking a list of trees, return a list of the results of interpretation
        -- of each individual tree.
        --
        -- Differs from program interpretation in that the list of trees in the Runtime
        -- is parallel and not serial, in concept.
        interpretArgumentList :: Env.Runtime -> IO [Syntax.SyntaxTree]
        interpretArgumentList = flip interpretArgumentList' (pure [])
          where
            interpretArgumentList' ::
              Env.Runtime -> IO [Syntax.SyntaxTree] -> IO [Syntax.SyntaxTree]
            interpretArgumentList' (Env.Runtime st [] err) accum =
              accum >>= return . reverse
            interpretArgumentList' (Env.Runtime st (tr' : trs') err) accum = do
              joinedAccum <- accum
              toCons <- argumentResult (Env.Runtime st [tr'] err)
              interpretArgumentList'
                (Env.Runtime st trs' err)
                (return (toCons : joinedAccum))
              where
                -- This is a partial function but hopefully
                -- it doesn't lead to any trouble later.
                argumentResult :: Env.Runtime -> IO Syntax.SyntaxTree
                argumentResult =
                  fmap (\(Env.Runtime _ xs _) -> (Maybe.fromJust . UGen.head') xs)
                    . interpret

treeHeadIsStandardLibraryCall :: Syntax.SyntaxTree -> Bool
treeHeadIsStandardLibraryCall =
  UGen.foldIdApplicativeOnSingleton
    all
    [ Inspect.treeHeadIsFunctionCall,
      Tree.nodeStrictlySatisfies nodeIsStandardLibCall
    ]

nodeIsStandardLibCall :: Syntax.SyntaxUnit -> Bool
nodeIsStandardLibCall su = case Syntax.token su of
  (Syntax.Data (Syntax.Id id)) ->
    (elem id . map SknStdLib.stdLibFunctionId) SknStdLib.exporting
  _ -> False

----Statement Processing Functions--------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Contains guards for processing individual statement trees.
--
-- operates on, but does not consume, the head of the current runtimeValue.
--
-- Should probably have guards for the statement types found in the Parser.
-- Currently missing a guard for the Shoal global statement.
processStatementTree :: Env.Runtime -> Env.Runtime
processStatementTree rt = processStatementTree' rt
  where
    processStatementTree' (Env.Runtime st (tr : trs) err)
      | Inspect.treeHeadIsFishDeclaration tr =
        processFishStatement rt
      | otherwise = rt

-- | Take a tree of a 'fish' declaration structure and return a new runtime with that
-- symbol injected into it.
--
-- relies on partial functions, will throw errors if called on trees that are not
-- valid 'fish' declaration structures.
--
-- Valid tree structure is as follows:
--
-- > Fish :-<-: Id :-<-: Lamprey :-<-: [...]
processFishStatement :: Env.Runtime -> Env.Runtime
processFishStatement rt = processFishStatement' rt
  where
    processFishStatement' (Env.Runtime st (tr : trs) err) =
      Env.injectBinding (makeFishBinding tr) rt
    makeFishBinding :: Syntax.SyntaxTree -> Env.Binding
    makeFishBinding tr =
      Env.Binding (Env.BindingKey (getFishId tr)) (getFishParams tr) (getFishLamprey tr)
      where
        getFishId :: Syntax.SyntaxTree -> String
        getFishId =
          Maybe.fromJust
            . ( \m ->
                  Syntax.unId
                    =<< (Syntax.baseData . Syntax.token)
                    =<< Tree.treeNode
                    =<< m
              )
            . UGen.head'
            . Tree.treeChildren
        getFishLamprey :: Syntax.SyntaxTree -> Syntax.SyntaxTree
        getFishLamprey =
          Maybe.fromJust
            . (=<<) (UGen.head' . Tree.treeChildren)
            . UGen.head'
            . Tree.treeChildren
        getFishParams :: Syntax.SyntaxTree -> [Syntax.SyntaxUnit]
        getFishParams =
          Maybe.mapMaybe Tree.treeNode
            . filter Inspect.treeHeadIsPositionalParameter
            . Tree.treeChildren

----Traverse and Retrieve Functions-------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Retrieve the positional parameters from a syntax tree whose parent node is a Lamprey.
--
-- Should work, but since it only checks the parent nodes of the children of Lamprey,
-- there may be some circumstances, though I don't foresee any, where it may yield some
-- unexpected results.
getLampreyPositionalParameters :: Syntax.SyntaxTree -> [Syntax.SyntaxUnit]
getLampreyPositionalParameters =
  UGen.filterFromJust
    (UGen.foldIdApplicativeOnSingleton all [Inspect.nodeIsId, Inspect.nodeIsSendContext])
    . map Tree.treeNode
    . Tree.treeChildren

-- | Retrieve the value (Return context tree) of a tree whose parent node is a Lamprey.
--
-- This function uses the partial function 'last' but it is guaranteed to work due to
-- the initial pass through the parser. That is, all Lamprey trees are guaranteed to have
-- one and only one Return child.
getLampreyValue :: Syntax.SyntaxTree -> Syntax.SyntaxTree
getLampreyValue = last . Tree.treeChildren

-- | Advances the runtimeValue from the current head to the next tree.
advanceToNextTree :: Env.Runtime -> Env.Runtime
advanceToNextTree rt =
  if (null . Env.runtimeValue) rt
    then rt
    else rt {Env.runtimeValue = (UGen.tail' . Env.runtimeValue) rt}

----General-------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

throwGeneralError :: a
throwGeneralError =
  Exception.raiseError
    (Exception.newException Exception.General [] "" Exception.Fatal)

generalException = Exception.newException Exception.General [] "" Exception.Fatal

throwGeneralErrorWithMsg msg =
  Exception.raiseError
    ( Exception.newException
        Exception.General
        []
        msg
        Exception.Fatal
    )

testString :: String -> IO (Either Syntax.SyntaxTree Syntax.Data)
testString = flip evaluateProgram [] . parseString

parseString = Parser.Main.parse "Interpreter.Main.tests"

sknString :: String
sknString = "fish main <( - >(10)> >(1)>)<"

printResult :: String -> IO ()
printResult str = testString str >>= Either.either UC.printf UC.printf