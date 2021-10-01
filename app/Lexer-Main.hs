import qualified Lexer (tokenize)
import System.Environment
import System.IO

printTokens :: String -> String
printTokens xs = (unlines . map show . Lexer.tokenize) xs

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStr $ printTokens contents
  hClose handle