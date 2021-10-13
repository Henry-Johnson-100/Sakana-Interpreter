import ExecutionTree (executeMain, calct')
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    handle <- openFile (head args) ReadMode
    contents <-hGetContents handle
    (print . executeMain . calct') contents
    hClose handle