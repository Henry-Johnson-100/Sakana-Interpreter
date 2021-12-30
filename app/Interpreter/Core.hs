module Interpreter.Core
  (
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
import qualified Syntax
import qualified System.IO as IO
import Util.General ((.<))
import qualified Util.General as UGen
import qualified Util.Tree as Tree
import Prelude hiding (lookup)

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
-- It would be best to store inline C or ccall imports in a tree and then store those
-- in the runtime. That way, the std_lib could be expanded by including more header files
-- or just adding more functions to an existing header.
evaluateStandardLibraryCall :: Env.Runtime -> IO Env.Runtime
evaluateStandardLibraryCall rt = evaluateStandardLibraryCall' rt
  where
    evaluateStandardLibraryCall' :: Env.Runtime -> IO Env.Runtime
    evaluateStandardLibraryCall' (Env.Runtime st (tr : trs) err) =
      case getBaseFunctionIdString tr of
        "+" -> return rt
        _ -> return rt
      where
        getBaseFunctionIdString :: Syntax.SyntaxTree -> String
        getBaseFunctionIdString =
          ( Maybe.fromJust
              . (=<<) Syntax.unId
              . (=<<) (Syntax.baseData . Syntax.token)
              . Tree.treeNode
          )

treeHeadIsStandardLibraryCall :: Syntax.SyntaxTree -> Bool
treeHeadIsStandardLibraryCall =
  UGen.foldIdApplicativeOnSingleton
    all
    [ Inspect.treeHeadIsFunctionCall,
      Tree.nodeStrictlySatisfies Interpreter.Core.nodeIsStandardLibCall
    ]

nodeIsStandardLibCall :: Syntax.SyntaxUnit -> Bool
nodeIsStandardLibCall su = case Syntax.token su of
  (Syntax.Data (Syntax.Id id)) -> elem id (SknStdLib.stdLibIds)
  _ -> False

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

----Inline Standard Library---------------------------------------------------------------
------------------------------------------------------------------------------------------
standardLibIdStrings :: [String]
standardLibIdStrings =
  [ "+",
    "-",
    "/",
    "*",
    "^",
    "==",
    "/=",
    ">",
    "<",
    ">=",
    "<=",
    "fin",
    "#INLINE#",
    "to_string",
    "to_bool",
    "concat_str",
    "read_prim",
    "floor"
  ]

getSakanaArgForInlineBody :: String
getSakanaArgForInlineBody = ""

getSakanaNumDraft :: Syntax.Data -> Double
getSakanaNumDraft (Syntax.Num n) = n
getSakanaNumDraft _ = throwGeneralError

getSakanaArgDraft :: Syntax.SyntaxTree -> Syntax.Data
getSakanaArgDraft tr = case Tree.treeNode tr of
  (Maybe.Just (Syntax.SyntaxUnit (Syntax.Data d) _ _)) -> d
  _ -> throwGeneralError

throwGeneralError :: a
throwGeneralError =
  Exception.raiseError
    (Exception.newException Exception.General [] "" Exception.Fatal)