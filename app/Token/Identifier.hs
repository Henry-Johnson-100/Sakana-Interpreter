module Token.Identifier(
Id(..),
readId,
fromId,
like,
notLike
) where

import Token.Util.Like

data Id = Id String deriving (Show,Read,Eq)

instance Like Id where
    (Id _) `like` (Id _) = True
    a      `notLike` b   = not $ like a b

readId :: String -> Id
readId str = Id str

fromId :: Id -> String
fromId (Id str) = str