module Interpreter
  ( interpret,
  )
where

import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified Interpreter.Core
import qualified Interpreter.Environment as Env
import qualified Interpreter.Inspection as Inspect
import qualified Syntax
import qualified System.IO as IO
import qualified Util.General as UGen
import qualified Util.Tree as Tree

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

treeHeadIsStandardLibraryCall :: Syntax.SyntaxTree -> Bool
treeHeadIsStandardLibraryCall =
  UGen.foldIdApplicativeOnSingleton
    all
    [ Inspect.treeHeadIsFunctionCall,
      Tree.nodeStrictlySatisfies Interpreter.Core.nodeIsStandardLibCall
    ]