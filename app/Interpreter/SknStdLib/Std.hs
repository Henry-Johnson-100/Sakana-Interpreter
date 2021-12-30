module Interpreter.SknStdLib.Std
  ( module Interpreter.SknStdLib.IO,
    stdLibIds,
  )
where

import Interpreter.SknStdLib.IO

stdLibIds :: [String]
stdLibIds = concat [Interpreter.SknStdLib.IO.exportingIds]