module Util.Classes
  ( Format (..),
    Emptiable (..),
    Like (..),
  )
where

import Util.General ((.<))

-- | A class for taking a data struct and producing a formatted string, for display to
-- an end user.
--
-- defines:
--
-- > format :: a -> String
-- > printf :: a -> IO ()
--
-- Minimal complete definition:
--
-- > format
class Format a where
  format :: a -> String
  printf :: a -> IO ()
  printf = putStrLn . format

{-- |
Format instances for common types.
-}

instance Format Double where
  format = show

instance Format Bool where
  format = show

instance Format Int where
  format = show

instance Format Integer where
  format = show

instance Show a => Format [a] where
  format = show

instance (Show a, Show b) => Format (a, b) where
  format = show

-- | For data types of kind * -> * with constraint Eq a => T a
--
-- defines
--
-- > empty :: (Emptiable a, Eq a) => a
--
-- > isEmpty :: (Emptiable a, Eq a) => a -> Bool
--
-- Minimal complete definition:
--
-- > empty
class Eq a => Emptiable a where
  empty :: a
  isEmpty :: a -> Bool
  isEmpty = (==) empty

class Like a where
  like :: a -> a -> Bool
  notLike :: a -> a -> Bool
  notLike = not .< like