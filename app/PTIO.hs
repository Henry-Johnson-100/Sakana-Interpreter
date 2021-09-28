import Lexer (tokenize)
import SyntaxTree (TreeIO (fPrintTree), generateSyntaxTree)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

printTree :: String -> String
printTree c = concatMap (0 `fPrintTree`) ((generateSyntaxTree . tokenize) c)

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStr $ printTree contents
  hClose handle