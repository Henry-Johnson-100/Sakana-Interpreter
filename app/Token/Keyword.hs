module Token.Keyword (
Keyword(..),
readKeyword,
fromKeyword,
repr
) where

data Keyword = Fish | Route | School | Shoal deriving (Show, Read, Eq, Ord) --idk if Ord is really necessary

repr :: [String]
repr = ["route","fish","school","shoal"]  

readKeyword :: String -> Keyword
readKeyword "fish"   = Fish
readKeyword "route"  = Route
readKeyword "school" = School
readKeyword "shoal"  = Shoal

fromKeyword :: Keyword -> String
fromKeyword Fish   = "fish"
fromKeyword Route  = "route"
fromKeyword School = "school"
fromKeyword Shoal  = "shoal"