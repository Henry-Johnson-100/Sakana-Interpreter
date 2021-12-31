{-# LANGUAGE MagicHash #-}

module Interpreter.SknStdLib.Type
  ( SknStdLibFunction (..),
    raiseSknStdLibArgumentException,
    generalStdLibFunctionParamNumber,
    evaluateGeneralStdLibFunction,
  )
where

import qualified Exception.Base as Exception
import qualified Syntax
import qualified Util.Classes as UC
import qualified Util.Tree as Tree

data SknStdLibFunction = GeneralStdLibFunction
  { generalStdLibFunctionId :: String,
    generalStdLibFunctionParams :: [String],
    generalStdLibFunctionDefinition ::
      [Syntax.SyntaxTree] ->
      IO Syntax.SyntaxTree
  }

generalStdLibFunctionParamNumber :: SknStdLibFunction -> Int
generalStdLibFunctionParamNumber = length . generalStdLibFunctionParams

exportingIds :: [String]
exportingIds = map generalStdLibFunctionId exportingFunctions

exportingFunctions :: [SknStdLibFunction]
exportingFunctions = [sakana_const]

evaluateGeneralStdLibFunction ::
  SknStdLibFunction -> [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
evaluateGeneralStdLibFunction genFunc trs = (generalStdLibFunctionDefinition genFunc) trs

-- | Here is a proposition as well as an example of how a std lib might be defined.
--
-- Notice the pattern matching to keep only the exact number of arguments expected
-- and throw an error in any other event.
-- this can be caught beforehand by inspecting comparing the SknStdLibFunction's
--
-- > generalStdLibFunctionParams
-- field with the provided arguments before any attempt to evaluate it has been made.
--
-- Any additional error handling can be done in the function definition proper.
sakana_const :: SknStdLibFunction
sakana_const =
  GeneralStdLibFunction "const" ["keep", "replace"] sakana_const#
  where
    sakana_const# :: [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
    sakana_const# args = sakana_const'# args
      where
        sakana_const'# (trKeep : trReplace : []) = return trKeep
        sakana_const'# _ =
          raiseSknStdLibArgumentException
            args
            "Expected only two arguments: \'keep\', \'replace\'"

raiseSknStdLibArgumentException :: [Syntax.SyntaxTree] -> [Char] -> a2
raiseSknStdLibArgumentException trs expectedArgMessage =
  Exception.raiseError
    ( Exception.newException
        Exception.FunctionArgumentNumberException
        ((map Syntax.line . concatMap Tree.flattenTree) trs)
        ( "Mismatched number of arguments in uncurryable function:\n"
            ++ expectedArgMessage
            ++ "\n Argument trees are: "
            ++ (unlines . map UC.format) trs
        )
        Exception.Fatal
    )