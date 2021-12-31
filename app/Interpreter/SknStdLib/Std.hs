module Interpreter.SknStdLib.Std
  ( module Interpreter.SknStdLib.Type,
    exporting,
  )
where

import qualified Interpreter.SknStdLib.IO
import Interpreter.SknStdLib.Type

exporting :: [SknStdLibFunction]
exporting = [] ++ Interpreter.SknStdLib.IO.exporting