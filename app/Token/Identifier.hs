module Token.Identifier(
Id(..),
readId,
fromId
) where

data Id = Id String deriving (Show,Read,Eq)

readId :: String -> Id
readId str = Id str

fromId :: Id -> String
fromId (Id str) = str