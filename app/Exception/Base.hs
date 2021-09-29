{-#LANGUAGE MagicHash #-}
module Exception.Base where

import Data.List (findIndex)
import Data.Ord (Ordering)
import System.Exit (die)
import GHC.Prim (raise#)
import GHC.Exception (errorCallException)

data ExceptionType
  = General
  | InvalidBracketing
  | InvalidID
  | InvalidArgs
  | UndefinedTokenException
  | IncompleteStringLiteralException
  | FishDeclarationMissingReturn
  deriving (Show, Eq)

data ExceptionSeverity
  = Fatal
  | NonFatal
  | Debug
  | Log
  deriving (Show, Eq, Ord)

data ExceptionInfo
  = NoInfo
  | ExceptionInfo
      { exceptionLine :: Int,
        exceptionMessage :: String,
        severity :: ExceptionSeverity
      }
  deriving (Eq)

data Exception = Exception
  { exceptionType :: ExceptionType,
    information :: ExceptionInfo
  }
  deriving (Eq)

instance Ord ExceptionType where
  compare x y
    | findIndex' x > findIndex' y = LT
    | findIndex' x < findIndex' y = GT
    | otherwise = EQ
    where
      findIndex' x' = findIndex (elem x') exceptionTypeOrder

instance Show ExceptionInfo where
  show NoInfo = "No Information."
  show (ExceptionInfo ln msg sev) = "On line " ++ show ln ++ ",\n" ++ show sev ++ " error, " ++ msg ++ "\n"

instance Ord Exception where
  compare x y
    | exceptionType x /= exceptionType y = compare (exceptionType x) (exceptionType y)
    | otherwise = compare ((severity . information) x) ((severity . information) y)

instance Show Exception where
  show (Exception et info) = show info

exceptionTypeOrder :: [[ExceptionType]]
exceptionTypeOrder =
  [ [General],
    [ UndefinedTokenException,
      IncompleteStringLiteralException,
      InvalidBracketing,
      InvalidID,
      InvalidArgs
    ]
  ]

setExceptionSeverity :: Exception -> ExceptionSeverity -> Exception
setExceptionSeverity e es =
  Exception
    { exceptionType = exceptionType e,
      information =
        ExceptionInfo
          { exceptionLine = (exceptionLine . information) e,
            exceptionMessage = (exceptionMessage . information) e,
            severity = es
          }
    }

newException :: ExceptionType -> Int -> String -> ExceptionSeverity -> Exception
newException et ln s es =
  Exception
    { exceptionType = et,
      information =
        ExceptionInfo
          { exceptionLine = ln,
            exceptionMessage = s,
            severity = es
          }
    }

raiseError :: Show a1 => a1 -> a2
raiseError exc = raise# (errorCallException (show exc))