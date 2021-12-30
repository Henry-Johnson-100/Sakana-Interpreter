module Syntax
  ( Keyword (..),
    Data (..),
    BracketTerminal (..),
    ScopeType (..),
    SyntaxTree (..),
    Source (..),
    SyntaxUnit (..),
    Token (..),
    TokenSource (..),
    isPrimitive,
    isString,
    isNumeric,
    unNum,
    unString,
    unBoolean,
    unId,
    keywords,
    keywordRequiresId,
    fromBracket,
    fromScopeType,
    fromTerminal,
    baseData,
    baseKeyword,
    getTokenBracketScopeType,
    genericKeyword,
    genericBracket,
    genericData,
    dataTokenIsId,
    keywordTokenIsDeclarationRequiringId,
    sourceToSyntaxUnit,
  )
where

import qualified Control.Monad as CMonad
import qualified Data.List
import qualified Data.Maybe as DMaybe
import qualified Util.Classes as UC
import Util.General ((.<))
import qualified Util.General as UGen
import qualified Util.Tree as Tree

----Data definitions----------------------------------------------------------------------
------------------------------------------------------------------------------------------

data Source a = Source {sourceUnit :: a, sourceLine :: Int} deriving (Show, Eq, Read)

data SyntaxUnit = SyntaxUnit
  { token :: Token,
    line :: Int,
    context :: ScopeType
  }
  deriving (Show, Eq)

data Token
  = Bracket ScopeType BracketTerminal
  | Data Data
  | Keyword Keyword
  deriving (Show, Read, Eq)

data BracketTerminal = Open | Close deriving (Show, Read, Eq)

data ScopeType = Send | Return deriving (Show, Read, Eq)

data Keyword
  = Fish
  | School
  | Shoal
  | Swim
  | Lamprey
  deriving (Show, Read, Eq)

data Data
  = Num Double
  | String String
  | Boolean Bool
  | Id String
  | Null
  deriving (Show, Read, Eq)

----Type synonyms-------------------------------------------------------------------------
------------------------------------------------------------------------------------------

type TokenSource = Source Token

type SyntaxTree = Tree.Tree SyntaxUnit

----Instances-----------------------------------------------------------------------------
------------------------------------------------------------------------------------------

----Like

instance UC.Like Data where
  (Num _) `like` (Num _) = True
  (String _) `like` (String _) = True
  (Boolean _) `like` (Boolean _) = True
  (Id _) `like` (Id _) = True
  Null `like` Null = True
  _ `like` _ = False

instance UC.Like Keyword where
  like = (==)

instance UC.Like Token where
  like (Bracket _ _) (Bracket _ _) = True
  like (Data _) (Data _) = True
  like (Keyword _) (Keyword _) = True
  like _ _ = False

----Defaultable

instance UC.Defaultable Data where
  defaultValue = Null

instance UC.Defaultable SyntaxUnit where
  defaultValue = SyntaxUnit (Data Null) 0 Return

----Format

instance UC.Format ScopeType where
  format = show

instance UC.Format BracketTerminal where
  format = show

instance UC.Format Data where
  format (String a) = a
  format (Num a) = show a
  format (Boolean a) = show a
  format (Id a) = a
  format Null = ""

instance UC.Format Keyword where
  format Fish = "fish"
  format School = "school"
  format Shoal = "shoal"
  format Swim = "swim"
  format Lamprey = "lamprey"

instance UC.Format Token where
  format (Bracket st bt) = fromBracket st bt
  format (Data d) = UC.format d
  format (Keyword k) = UC.format k

instance (UC.Format a) => UC.Format (Source a) where
  format (Source x l) = UC.format x ++ ": " ++ UC.format l

instance UC.Format SyntaxUnit where
  format (SyntaxUnit t l c) =
    Data.List.intercalate " | " $ [UC.format t, UC.format l, UC.format c]

----Data functions------------------------------------------------------------------------
------------------------------------------------------------------------------------------

primTypeOf :: Data -> String
primTypeOf (String _) = "String"
primTypeOf (Num _) = "Num"
primTypeOf (Boolean _) = "Boolean"
primTypeOf (Id _) = "Id"
primTypeOf Null = "Null"

isPrimitive :: Data -> Bool
isPrimitive d = any (d `UC.like`) [Num 0.0, String "", Boolean True, Null]

isNumeric :: Data -> Bool
isNumeric = UC.like (Num 0.0)

isString :: Data -> Bool
isString (String _) = True
isString _ = False

unNum :: Data -> Maybe Double
unNum (Num x) = Just x
unNum Null = Just 0.0
unNum _ = Nothing

unString :: Data -> Maybe String
unString (String s) = Just s
unString Null = Just ""
unString _ = Nothing

unBoolean :: Data -> Maybe Bool
unBoolean (Boolean b) = Just b
unBoolean Null = Just False
unBoolean _ = Nothing

unId :: Data -> Maybe String
unId (Id d) = Just d
unId _ = Nothing

----Keyword functions---------------------------------------------------------------------
------------------------------------------------------------------------------------------

keywords :: [String]
keywords = map UC.format [Fish, School, Shoal, Swim, Lamprey]

keywordRequiresId :: Keyword -> Bool
keywordRequiresId k = if elem k [Swim, Lamprey] then False else True

----Bracket functions---------------------------------------------------------------------
------------------------------------------------------------------------------------------

fromScopeType :: ScopeType -> Char
fromScopeType Send = '>'
fromScopeType Return = '<'

fromTerminal :: BracketTerminal -> Char
fromTerminal Open = '('
fromTerminal Close = ')'

fromBracket :: ScopeType -> BracketTerminal -> String
fromBracket =
  CMonad.liftM2
    (:)
    (fromScopeType . fst)
    ( UGen.listSingleton
        . fromTerminal
        . snd
    )
    .< (,)

----Token functions-----------------------------------------------------------------------
------------------------------------------------------------------------------------------

baseData :: Token -> DMaybe.Maybe Data
baseData (Data d) = DMaybe.Just d
baseData _ = DMaybe.Nothing

baseKeyword :: Token -> DMaybe.Maybe Keyword
baseKeyword (Keyword k) = DMaybe.Just k
baseKeyword _ = DMaybe.Nothing

getTokenBracketScopeType :: Token -> DMaybe.Maybe ScopeType
getTokenBracketScopeType (Bracket st _) = DMaybe.Just st
getTokenBracketScopeType _ = DMaybe.Nothing

genericKeyword :: Token
genericKeyword = Keyword Fish

genericBracket :: Token
genericBracket = Bracket Send Open

genericData :: Token
genericData = Data (Num 0)

dataTokenIsId :: Token -> Bool
dataTokenIsId (Data (Id _)) = True
dataTokenIsId _ = False

keywordTokenIsDeclarationRequiringId :: Token -> Bool
keywordTokenIsDeclarationRequiringId t =
  DMaybe.maybe False (keywordRequiresId) (baseKeyword t)

----Source functions----------------------------------------------------------------------
------------------------------------------------------------------------------------------

sourceToSyntaxUnit :: TokenSource -> ScopeType -> SyntaxUnit
sourceToSyntaxUnit (Source t ln) = SyntaxUnit t ln