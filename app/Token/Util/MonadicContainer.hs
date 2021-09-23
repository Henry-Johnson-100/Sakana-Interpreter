module Token.Util.MonadicContainer where

newtype MonadicContainer a = MonadicContainer a deriving (Show, Eq)

instance Functor MonadicContainer where
  fmap f (MonadicContainer x) = MonadicContainer (f x)

instance Applicative MonadicContainer where
  pure = MonadicContainer
  (MonadicContainer f) <*> (MonadicContainer x) = MonadicContainer (f x)

instance Monad MonadicContainer where
  return = MonadicContainer
  (MonadicContainer x) >>= f = f x

put :: a -> MonadicContainer a
put = MonadicContainer

get :: MonadicContainer a -> a
get (MonadicContainer x) = x