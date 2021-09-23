import Lexer (tokenize)
import ParseTree (TreeIO (fPrintTree), generateParseTree)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

printTree :: String -> String
printTree c = fPrintTree 0 (generateParseTree (tokenize c))

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStr $ printTree contents
  hClose handle