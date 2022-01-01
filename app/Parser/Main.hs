module Parser.Main
  ( parse,
  )
where

import Data.Either (either)
import Exception.Base (raiseError)
import Parser.Core (getParseError, parse')
import Parser.Syntax (SyntaxTree)
import Text.Parsec (SourceName)
import Util.General ((.<))

parse :: SourceName -> [Char] -> SyntaxTree
parse srcName srcString =
  either (raiseError . flip getParseError srcString) id . parse' srcName $ srcString