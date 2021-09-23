module Exception
  ( ExceptionType (..),
    ExceptionSeverity (..),
    ExceptionInfo (..),
    Exception (..),
    throw,
    setExceptionSeverity,
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
      { --line :: Int,
        exceptionMessage :: String,
        severity :: ExceptionSeverity
      }
  deriving (Show, Eq)

data Exception = Exception
  { exceptionType :: ExceptionType,
    information :: ExceptionInfo
  }
  deriving (Show, Eq)

instance Ord Exception where
  compare x y
    | exceptionType x /= exceptionType y = compare (exceptionType x) (exceptionType y)
    | otherwise = compare (severity (information x)) (severity (information y))

setExceptionSeverity :: Exception -> ExceptionSeverity -> Exception
setExceptionSeverity e es =
  Exception
    { exceptionType = exceptionType e,
      information =
        ExceptionInfo
          { exceptionMessage = exceptionMessage (information e),
            severity = es
          }
    }

throw :: Exception -> IO ()
throw e
  | severity (information e) == Fatal = die (show e)
  | otherwise = print e --Include logging or something later idk