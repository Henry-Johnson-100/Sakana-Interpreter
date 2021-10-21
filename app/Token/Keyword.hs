module Token.Keyword
  ( Keyword (..),
    readKeyword,
    fromKeyword,
    repr,
    isDeclarationRequiringId,
  )
where

data Keyword
  = Fish
  | Route
  | School
  | Shoal
  | Migrate
  | Swim
  deriving (Show, Read, Eq, Ord) --idk if Ord is really necessary

repr :: [String]
repr = ["route", "fish", "school", "shoal", "migrate", "swim"]

isDeclarationRequiringId :: Keyword -> Bool
isDeclarationRequiringId Migrate = False
isDeclarationRequiringId Swim = False
isDeclarationRequiringId _ = True

readKeyword :: String -> Keyword
readKeyword "fish" = Fish
readKeyword "route" = Route
readKeyword "school" = School
readKeyword "shoal" = Shoal
readKeyword "migrate" = Migrate
readKeyword "swim" = Swim

fromKeyword :: Keyword -> String
fromKeyword Fish = "fish"
fromKeyword Route = "route"
fromKeyword School = "school"
fromKeyword Shoal = "shoal"
fromKeyword Migrate = "migrate"
fromKeyword Swim = "swim"