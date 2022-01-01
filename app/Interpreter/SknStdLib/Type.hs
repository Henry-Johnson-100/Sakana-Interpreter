{-# LANGUAGE MagicHash #-}

module Interpreter.SknStdLib.Type
  ( SknStdLibFunction (..),
    raiseSknStdLibArgumentException,
    generalStdLibFunctionParamNumber,
    evaluateGeneralStdLibFunction,
  )
where

import qualified Exception.Base as Exception
import qualified Parser.Syntax as Syntax
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

evaluateGeneralStdLibFunction ::
  SknStdLibFunction -> [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
evaluateGeneralStdLibFunction genFunc trs = (generalStdLibFunctionDefinition genFunc) trs

raiseSknStdLibArgumentException :: [Syntax.SyntaxTree] -> [Char] -> [String] -> a2
raiseSknStdLibArgumentException trs expectedArgMessage expectedArgIds =
  Exception.raiseError
    ( Exception.newException
        Exception.FunctionArgumentNumberException
        ((map Syntax.line . concatMap Tree.flattenTree) trs)
        ( "Mismatched number of arguments in uncurryable function:\n"
            ++ expectedArgMessage
            ++ "\n Argument trees are: "
            ++ "\n Expecting only the following arguments: "
            ++ unwords expectedArgIds
            ++ (unlines . map UC.format) trs
        )
        Exception.Fatal
    )

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
  GeneralStdLibFunction "const" sakana_const_params# sakana_const#
  where
    sakana_const_params# :: [String]
    sakana_const_params# = ["keep", "replace"]
    sakana_const# :: [Syntax.SyntaxTree] -> IO Syntax.SyntaxTree
    -- Note that it may be preferred to have this double nested 'where'
    -- to keep an easy reference to the list of trees passed into the function
    -- for error reporting.
    sakana_const# args = sakana_const'# args
      where
        sakana_const'# (trKeep : trReplace : []) = return trKeep
        sakana_const'# _ =
          raiseSknStdLibArgumentException
            args
            ""
            sakana_const_params#