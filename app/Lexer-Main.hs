import Lexer (tokenize)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

printTokens :: String -> String
printTokens xs = unlines $ map show $ tokenize xs

main :: IO ()
main = do
  args <- getArgs
  handle <- openFile (head args) ReadMode
  contents <- hGetContents handle
  putStr $ printTokens contents
  hClose handle