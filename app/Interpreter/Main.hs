module Interpreter.Main
  ( preprocessParserOutput,
    createCLIArgumentBindings,
    evaluateProgram,
  )
where

import qualified Control.Monad as CMonad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List hiding (lookup)
import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified Interpreter.Environment as Env
import qualified Interpreter.Inspection as Inspect
import qualified Interpreter.SknStdLib.Std as SknStdLib
import qualified Parser.Syntax as Syntax
import qualified System.IO as IO
import qualified Util.Classes as UC
import Util.General ((.<))
import qualified Util.General as UGen
import Util.Tree (Tree ((:-<-:)))
import qualified Util.Tree as Tree
import Prelude hiding (lookup)

-- | Takes the raw output from the parser and converts it into an interpretable Runtime.
preprocessParserOutput :: Syntax.SyntaxTree -> [Env.Binding] -> Env.Runtime
preprocessParserOutput docTree additionalBindings =
  ( setMain
      . injectAdditionalBindings additionalBindings
      . foldDocTreeChildrenToRuntime
  )
    UC.defaultValue {Env.runtimeValue = Tree.treeChildren docTree}
  where
    foldDocTreeChildrenToRuntime :: Env.Runtime -> Env.Runtime
    foldDocTreeChildrenToRuntime rt =
      if (null . Env.runtimeValue) rt
        then rt
        else (foldDocTreeChildrenToRuntime . advanceToNextTree . processStatementTree) rt
    -- Finds the function identified as 'main' in the runtime's symbol table
    -- and returns a runtime with that function as the runtimeValue.
    setMain :: Env.Runtime -> Env.Runtime
    setMain rt =
      Maybe.maybe
        (Env.replaceException noMainException rt)
        (flip Env.replaceValue rt . UGen.listSingleton)
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

createCLIArgumentBindings :: [String] -> [Env.Binding]
createCLIArgumentBindings = map bindingFromTuple . zip argNameScheme
  where
    argNameScheme :: [String]
    argNameScheme = ["_arg_" ++ [c] | c <- ['A' .. 'z']]
    bindingFromTuple :: (String, String) -> Env.Binding
    bindingFromTuple (bid, bval) =
      Env.Binding
        (Env.BindingKey bid)
        (Tree.tree (UC.defaultValue {Syntax.token = Syntax.Data (Syntax.String bval)}))

-- | ...
evaluateProgram :: Env.Runtime -> IO Syntax.SyntaxTree
evaluateProgram = fmap programOutputHead . interpret
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

interpret :: Env.Runtime -> IO Env.Runtime
interpret rt = interpret' . Env.throwJustError $ rt
  where
    interpret' :: Env.Runtime -> IO Env.Runtime
    -- Will cause an infinite hang if Env.throwJustError is not present in interpret.
    interpret' (Env.Runtime st [] err) =
      interpret
        ( Env.replaceException
            ( Exception.newException
                Exception.NullTree
                []
                "Unexpected null tree in the interpreter."
                Exception.Fatal
            )
            rt
        )
    -- Guards go here.
    -- trs could be null.
    interpret' (Env.Runtime st (tr : trs) err)
      | Inspect.treeHeadIsPrimitiveData tr = return rt
      | treeHeadIsStandardLibraryCall tr = return rt
      | otherwise = return rt

-- | #TODO
evaluateFunction :: Env.Runtime -> IO Env.Runtime
evaluateFunction rt = evaluateFunction' rt
  where
    evaluateFunction' :: Env.Runtime -> IO Env.Runtime
    evaluateFunction' (Env.Runtime st (tr : trs) err)
      | Tree.nodeStrictlySatisfies nodeIsStandardLibCall tr = return rt
      | otherwise = return rt

-- | #TODO
-- still requires implementing argument number error checking and stuff
-- but I think it works for now
evaluateStandardLibraryCall :: Env.Runtime -> IO Env.Runtime
evaluateStandardLibraryCall rt = evaluateStandardLibraryCall' rt
  where
    evaluateStandardLibraryCall' :: Env.Runtime -> IO Env.Runtime
    evaluateStandardLibraryCall' (Env.Runtime st (tr : trs) err) = do
      evaluatedArguments <- interpretArgumentList (Env.replaceValue trs rt)
      stdLibFuncResult <-
        ( SknStdLib.generalStdLibFunctionDefinition
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
              . filter ((==) funcId . SknStdLib.generalStdLibFunctionId)
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
            interpretArgumentList' (Env.Runtime st [] err) accum = accum
            interpretArgumentList' (Env.Runtime st (tr' : trs') err) accum = do
              joinedAccum <- accum
              toCons <- argumentResult (Env.Runtime st [tr'] err)
              (return . flip (:) joinedAccum) toCons
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
    (elem id . map SknStdLib.generalStdLibFunctionId) SknStdLib.exporting
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
      Env.Binding (Env.BindingKey (getFishId tr)) (getFishLamprey tr)
      where
        getFishId :: Syntax.SyntaxTree -> String
        getFishId =
          Maybe.fromJust
            . (=<<) Syntax.unId
            . (=<<) (Syntax.baseData . Syntax.token)
            . Tree.treeNode
        getFishLamprey :: Syntax.SyntaxTree -> Syntax.SyntaxTree
        getFishLamprey =
          Maybe.fromJust
            . (=<<) (UGen.head' . Tree.treeChildren)
            . UGen.head'
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

throwGeneralErrorWithMsg msg =
  Exception.raiseError
    ( Exception.newException
        Exception.General
        []
        msg
        Exception.Fatal
    )