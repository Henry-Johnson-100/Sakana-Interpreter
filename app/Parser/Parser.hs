module Parser
  ( parse,
  )
where

import Data.Either (either)
import Exception.Base (raiseError)
import Parser.Core (getParseError, parse')
import Syntax (SyntaxTree)
import Text.Parsec (SourceName)
import Util.General ((.<))

parse :: SourceName -> [Char] -> SyntaxTree
parse = either (raiseError . getParseError) id .< parse'