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


data ExceptionSeverity = Fatal | NonFatal | Debug | Log deriving (Show,Eq,Ord)


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


exception :: ExceptionType -> Exception
exception InvalidBracketing = Exception {
    exceptionType = InvalidBracketing,
    information = ExceptionInfo {
        exceptionMessage = "Invalid Bracketing",
        severity = Fatal
    }
}
exception InvalidID = Exception {
    exceptionType = InvalidID,
    information = ExceptionInfo {
        exceptionMessage = "Invalid ID for function",
        severity = Fatal
    }
}
exception InvalidArgs = Exception {
    exceptionType = InvalidArgs,
    information = ExceptionInfo {
        exceptionMessage = "Invalid number of arguments in brackets",
        severity = Fatal
    }
}

throw :: Exception -> IO ()
throw e
    | (severity (information e)) == Fatal = die (show e)
    | otherwise                                       = putStrLn (show e) --Include logging or something later idk