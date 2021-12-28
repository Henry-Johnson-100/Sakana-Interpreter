module Util.Classes
  ( Format (..),
    Defaultable (..),
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
-- > defaultValue :: (Defaultable a, Eq a) => a
--
-- > isDefault :: (Defaultable a, Eq a) => a -> Bool
--
-- Minimal complete definition:
--
-- > defaultValue
class Eq a => Defaultable a where
  defaultValue :: a
  isDefault :: a -> Bool
  isDefault = (==) defaultValue

class Like a where
  like :: a -> a -> Bool
  notLike :: a -> a -> Bool
  notLike = not .< like