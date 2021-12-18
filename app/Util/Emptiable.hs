module Util.Emptiable
  ( Emptiable (..),
  )
where

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
