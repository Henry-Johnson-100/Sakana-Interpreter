import Lexer (tokenize)
import SyntaxTree (generateSyntaxTree)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
import Util.Tree (TreeIO (fPrintTree))

printTree :: String -> String
printTree fileContents =
  fPrintTree 0 ((generateSyntaxTree . tokenize) fileContents)

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStr $
    printTree
      contents
  hClose handle