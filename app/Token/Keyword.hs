module Token.Keyword (
Keyword(..),
readKeyword,
fromKeyword,
repr
) where

data Keyword = Hook | Fish | Route | School | Shoal | Migrate deriving (Show, Read, Eq, Ord) --idk if Ord is really necessary

repr :: [String]
repr = ["route","fish","school","shoal", "migrate", "hook"]  

readKeyword :: String -> Keyword
readKeyword "fish"    = Fish
readKeyword "route"   = Route
readKeyword "school"  = School
readKeyword "shoal"   = Shoal
readKeyword "hook"    = Hook
readKeyword "migrate" = Migrate

fromKeyword :: Keyword -> String
fromKeyword Fish    = "fish"
fromKeyword Route   = "route"
fromKeyword School  = "school"
fromKeyword Shoal   = "shoal"
fromKeyword Migrate = "migrate"
fromKeyword Hook    = "hook"