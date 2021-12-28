{-# LANGUAGE MagicHash #-}

module Exception.Base where

import qualified Data.List
import qualified GHC.Exception
import qualified GHC.Prim
import qualified Util.Classes as UC

data ExceptionType
  = General
  | FailedToParse
  | InvalidID
  | InvalidArgs
  | SymbolNotFound
  | SymbolIsAlreadyBound
  | ImproperBindingLookup
  deriving (Show, Eq, Ord)

data ExceptionSeverity
  = Fatal
  | NonFatal
  | Debug
  | Log
  deriving (Show, Eq, Ord)

data ExceptionInfo
  = NoInfo
  | ExceptionInfo
      { exceptionLines :: [Int],
        exceptionMessage :: String,
        severity :: ExceptionSeverity
      }
  deriving (Eq)

data Exception = Exception
  { exceptionType :: ExceptionType,
    information :: ExceptionInfo
  }
  deriving (Eq)

instance UC.Defaultable Exception where
  defaultValue = newException General [] "" Fatal

instance Show ExceptionInfo where
  show NoInfo = "No Information."
  show (ExceptionInfo ln msg sev) =
    "On line(s) "
      ++ ( case length ln of
             0 -> show 0
             1 -> (show . head) ln
             _ -> concat [(show . head) ln, "..", (show . last) ln]
         )
      ++ ",\n"
      ++ show sev
      ++ " error, "
      ++ msg
      ++ "\n"

instance Show Exception where
  show (Exception et info) = show info

setExceptionSeverity :: Exception -> ExceptionSeverity -> Exception
setExceptionSeverity e es =
  Exception
    { exceptionType = exceptionType e,
      information =
        ExceptionInfo
          { exceptionLines = (exceptionLines . information) e,
            exceptionMessage = (exceptionMessage . information) e,
            severity = es
          }
    }

newException :: ExceptionType -> [Int] -> String -> ExceptionSeverity -> Exception
newException et ln s es =
  Exception
    { exceptionType = et,
      information =
        ExceptionInfo
          { exceptionLines = ln,
            exceptionMessage = s,
            severity = es
          }
    }

raiseError :: Show a1 => a1 -> a2
raiseError exc = GHC.Prim.raise# (GHC.Exception.errorCallException (show exc))