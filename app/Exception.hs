module Exception (

) where

import System.Exit
import Data.List (findIndex)
import Data.Ord (Ordering)

data ExceptionType = General | InvalidBracketing | InvalidID | InvalidArgs deriving (Show,Eq)

exceptionTypeOrder :: [[ExceptionType]]
exceptionTypeOrder = [[General], [InvalidBracketing, InvalidID, InvalidArgs]]

instance Ord ExceptionType where
    compare x y
        | findIndex' x > findIndex' y = LT
        | findIndex' x < findIndex' y = GT
        | otherwise                   = EQ
        where
            findIndex' x' = findIndex (\e -> elem x' e) exceptionTypeOrder


data ExceptionSeverity = ExceptionSeverity {
    severityLevelName :: String,
    severityLevel     :: Int
} deriving (Show,Eq,Ord)


instance Bounded ExceptionSeverity where
    minBound = ExceptionSeverity "Log" 1
    maxBound = ExceptionSeverity "Fatal" 4


constructExceptionSeverityFromName :: String -> ExceptionSeverity
constructExceptionSeverityFromName "Fatal"    = ExceptionSeverity "Fatal"    4
constructExceptionSeverityFromName "NonFatal" = ExceptionSeverity "NonFatal" 3
constructExceptionSeverityFromName "Debug"    = ExceptionSeverity "Debug"    2
constructExceptionSeverityFromName "Log"      = ExceptionSeverity "Log"      1
constructExceptionSeverityFromName s          = ExceptionSeverity s          3


constructExceptionSeverityFromLevel :: Int -> ExceptionSeverity
constructExceptionSeverityFromLevel 4 = ExceptionSeverity "Fatal"    4
constructExceptionSeverityFromLevel 3 = ExceptionSeverity "NonFatal" 3
constructExceptionSeverityFromLevel 2 = ExceptionSeverity "Debug"    2
constructExceptionSeverityFromLevel _ = ExceptionSeverity "Log"      1


data ExceptionInfo = NoInfo | ExceptionInfo {
    --line :: Int,
    exceptionMessage :: String,
    severity :: ExceptionSeverity
} deriving (Show, Eq)


data Exception = Exception {
    exceptionType :: ExceptionType,
    information   :: ExceptionInfo
} deriving(Show, Eq)


instance Ord Exception where
    compare x y
        | exceptionType x /= exceptionType y = compare (exceptionType x) (exceptionType y)
        | otherwise                          = compare (severity (information x)) (severity (information y))


constructException :: ExceptionType -> Exception
constructException InvalidBracketing = Exception {
    exceptionType = InvalidBracketing,
    information = ExceptionInfo {
        exceptionMessage = "Invalid Bracketing",
        severity = constructExceptionSeverityFromLevel 4
    }
}
constructException InvalidID = Exception {
    exceptionType = InvalidID,
    information = ExceptionInfo {
        exceptionMessage = "Invalid ID for function",
        severity = constructExceptionSeverityFromLevel 4
    }
}
constructException InvalidArgs = Exception {
    exceptionType = InvalidArgs,
    information = ExceptionInfo {
        exceptionMessage = "Invalid number of arguments in brackets",
        severity = constructExceptionSeverityFromLevel 4
    }
}

throw :: Exception -> IO ()
throw e
    | (severityLevel (severity (information e))) == 4 = die (show e)
    | otherwise                                       = putStrLn (show e) --Include logging or something later idk