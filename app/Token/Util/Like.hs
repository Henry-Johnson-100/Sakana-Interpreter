module Token.Util.Like
  ( Like (..),
  )
where

class Like a where
  like :: a -> a -> Bool
  notLike :: a -> a -> Bool