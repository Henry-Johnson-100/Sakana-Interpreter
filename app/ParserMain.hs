import SakanaParser
import System.Environment
import System.IO

main = do
  args <- getArgs
  fileHandle <- openFile (head args) ReadMode
  fileStream <- hGetContents fileHandle
  parseResult <- return $ generateSyntaxTreeMain (head args) (fileStream)
  putStrLn (show parseResult)
  hClose fileHandle