module Token.Control
  ( Control (..),
    readControl,
    fromControl,
    repr,
  )
where

data Control = Fin deriving (Show, Read, Eq)

repr :: [String]
repr = ["fin"]

readControl :: String -> Control
readControl "fin" = Fin

fromControl :: Control -> String
fromControl Fin = "fin"