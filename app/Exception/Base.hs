module Exception.Base
  ( ExceptionType (..),
    ExceptionSeverity (..),
    ExceptionInfo (..),
    Exception (..),
    newUndefinedTokenException,
    setExceptionSeverity,
    raiseError,
  )
where

import Data.List (findIndex)
import Data.Ord (Ordering)
import System.Exit (die)

data ExceptionType = General | InvalidBracketing | InvalidID | InvalidArgs deriving (Show, Eq)

exceptionTypeOrder :: [[ExceptionType]]
exceptionTypeOrder = [[General], [InvalidBracketing, InvalidID, InvalidArgs]]

instance Ord ExceptionType where
  compare x y
    | findIndex' x > findIndex' y = LT
    | findIndex' x < findIndex' y = GT
    | otherwise = EQ
    where
      findIndex' x' = findIndex (elem x') exceptionTypeOrder

data ExceptionSeverity = Fatal | NonFatal | Debug | Log deriving (Show, Eq, Ord)

data ExceptionInfo
  = NoInfo
  | ExceptionInfo
      { exceptionLine :: Int,
        exceptionMessage :: String,
        severity :: ExceptionSeverity
      }
  deriving (Eq)

instance Show ExceptionInfo where
  show NoInfo = "No Information."
  show (ExceptionInfo ln msg sev) = "On line " ++ show ln ++ ", " ++ show sev ++ " error: " ++ msg ++ "\n"

data Exception = Exception
  { exceptionType :: ExceptionType,
    information :: ExceptionInfo
  }
  deriving (Eq)

instance Ord Exception where
  compare x y
    | exceptionType x /= exceptionType y = compare (exceptionType x) (exceptionType y)
    | otherwise = compare (severity (information x)) (severity (information y))

instance Show Exception where
  show (Exception et info) = show info

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

newFunctionBindArgException :: Int -> String -> Exception
newFunctionBindArgException ln s =
  Exception
    { exceptionType = InvalidArgs,
      information =
        ExceptionInfo
          { exceptionLine = ln,
            exceptionMessage = s,
            severity = Fatal
          }
    }

newFunctionBindIDException :: Int -> String -> Exception
newFunctionBindIDException ln s =
  Exception
    { exceptionType = InvalidID,
      information =
        ExceptionInfo
          { exceptionLine = ln,
            exceptionMessage = s,
            severity = Fatal
          }
    }

newUndefinedTokenException :: Int -> String -> Exception
newUndefinedTokenException ln s =
  Exception
    { exceptionType = General,
      information =
        ExceptionInfo
          { exceptionLine = ln,
            exceptionMessage = s,
            severity = Fatal
          }
    }

raiseError :: Show a1 => a1 -> a2
raiseError exc = error (show exc)