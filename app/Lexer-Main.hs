import System.IO
import System.Environment
import Lexer

printTokens xs = unlines $ map (show) $ tokenize xs

main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStr $ printTokens contents
    hClose handle