module Interpreter.SknStdLib.IO
  ( exportingIds,
    trout,
    herring,
    dolphin,
  )
where

import qualified Syntax
import qualified System.IO
import qualified Util.Classes as UC
import qualified Util.Tree as Tree

exportingIds :: [String]
exportingIds = ["trout", "dolphin", "herring"]

trout :: String -> IO Syntax.SyntaxTree
trout out = do
  System.IO.hPutStr System.IO.stdout out
  (return . Tree.tree) UC.defaultValue

herring :: String -> IO Syntax.SyntaxTree
herring errOut = do
  System.IO.hPutStr System.IO.stderr errOut
  (return . Tree.tree) UC.defaultValue

dolphin :: IO Syntax.SyntaxTree
dolphin = do
  inString <- System.IO.hGetContents System.IO.stdin
  let stringDataTree =
        Tree.tree UC.defaultValue {Syntax.token = Syntax.Data (Syntax.String inString)}
  return stringDataTree
