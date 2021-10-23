import Lexer (tokenize)
import SyntaxTree (generateModuleTree)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
import Util.Tree (TreeIO (fPrintTree))

printTree :: String -> String -> String
printTree fileContents treeName =
  fPrintTree 0 ((generateModuleTree treeName . tokenize) fileContents)

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStr $
    printTree
      contents
      (if (not . null . tail) args then (head . tail) args else "main")
  hClose handle