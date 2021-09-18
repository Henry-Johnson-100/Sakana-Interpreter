module Exception (

) where

import System.Exit
import Data.List (findIndex)
import Data.Ord (Ordering)

data ExceptionType = General | InvalidBracketing | InvalidID | InvalidArgs deriving (Show,Read,Eq)

exceptionTypeOrder :: [[ExceptionType]]
exceptionTypeOrder = [[General], [InvalidBracketing, InvalidID, InvalidArgs]]

instance Ord ExceptionType where
    compare x y
        | findIndex' x > findIndex' y = LT
        | findIndex' x < findIndex' y = GT
        | otherwise                   = EQ
        where
            findIndex' x' = findIndex (\e -> elem x' e) exceptionTypeOrder