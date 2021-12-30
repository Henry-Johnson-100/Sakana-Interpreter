module Interpreter.SknStdLib.IO
  ( exportingIds,
    trout,
    herring,
    dolphin,
  )
where

import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified Interpreter.Inspection
import qualified Syntax
import System.IO (hGetContents, hPutStr, stderr, stdin, stdout)
import qualified Util.Classes as UC
import qualified Util.General as UGen
import Util.Tree (Tree ((:-<-:)))
import qualified Util.Tree as Tree

exportingIds :: [String]
exportingIds = ["trout", "herring", "dolphin"]

-- | Pass in the result of an interpretation to be printed to stdout.
trout :: Syntax.SyntaxTree -> IO Syntax.SyntaxTree
trout ((Syntax.SyntaxUnit (Syntax.Data (Syntax.String str)) _ _) :-<-: []) =
  hPutStr stdout str >> return UC.defaultValue
trout tr = (Exception.raiseError . printTypeException) tr

-- | Pass in the result of interpretation to be printed to stderr.
herring :: Syntax.SyntaxTree -> IO Syntax.SyntaxTree
herring ((Syntax.SyntaxUnit (Syntax.Data (Syntax.String str)) _ _) :-<-: []) =
  hPutStr stderr str >> return UC.defaultValue
herring tr = (Exception.raiseError . printTypeException) tr

-- | Collect contents from stdin as a String.
dolphin :: IO Syntax.SyntaxTree
dolphin = do
  inString <- hGetContents stdin
  let stringDataTree =
        Tree.tree UC.defaultValue {Syntax.token = Syntax.Data (Syntax.String inString)}
  return stringDataTree

printTypeException :: Syntax.SyntaxTree -> Exception.Exception
printTypeException tr =
  Exception.newException
    Exception.TypeException
    ( ( map Syntax.line
          . Tree.flattenTree
      )
        tr
    )
    ("Error printing tree: " ++ UC.format tr ++ "\n Not a string primitive type.")
    Exception.Fatal