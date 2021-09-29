import Lexer (tokenize)
import SyntaxTree (TreeIO (fPrintTree), generateModuleTree)
import System.Environment (getArgs)
import System.IO

printTree :: String -> String -> String
printTree fileContents treeName = fPrintTree 0 ((generateModuleTree treeName . tokenize) fileContents)

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStr $ printTree contents (if (not . null . tail) args then (head . tail) args else "main")
  hClose handle