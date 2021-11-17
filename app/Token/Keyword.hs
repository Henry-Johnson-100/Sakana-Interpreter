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
  | Lamprey
  deriving (Show, Read, Eq, Ord) --idk if Ord is really necessary

repr :: [String]
repr = ["route", "fish", "lamprey", "school", "shoal", "migrate", "swim"]

isDeclarationRequiringId :: Keyword -> Bool
isDeclarationRequiringId k = if elem k [Swim, Migrate, Lamprey] then False else True

readKeyword :: String -> Keyword
readKeyword "fish" = Fish
readKeyword "route" = Route
readKeyword "school" = School
readKeyword "shoal" = Shoal
readKeyword "migrate" = Migrate
readKeyword "swim" = Swim
readKeyword "lamprey" = Lamprey

fromKeyword :: Keyword -> String
fromKeyword Fish = "fish"
fromKeyword Route = "route"
fromKeyword School = "school"
fromKeyword Shoal = "shoal"
fromKeyword Migrate = "migrate"
fromKeyword Swim = "swim"
fromKeyword Lamprey = "lamprey"