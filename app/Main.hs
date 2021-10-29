import Data.Version (Version (..), showVersion)
import SakanaParser (generateSyntaxTree)
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    hPutStrLn,
    openFile,
    stdout,
  )
import qualified Token.Data as D (fromData)
import TreeInterpreter (executeMain, getMainEnvironmentStack, getMainExecutionTrees)
import qualified Util.General (tail')

main :: IO ()
main = do
  args <- getArgs
  usingArgs args

usingArgs :: [String] -> IO ()
usingArgs args
  | argsContainAny ["-h", "--help"] =
    putStrLn
      "Usage: Sakana [path] [-v, --version] [-h, --help]\n\
      \\tPath:\n\
      \\t\tPath to the file to interpret.\n\
      \\tFlags:\n\
      \\t-v, --version\n\
      \\t\tDisplays the version of the Sakana Interpreter.\n\
      \\t-h, --help\n\
      \\t\tDisplays this help."
  | argsContainAny ["-v", "--version"] =
    putStrLn $ "Sakana Interpeter Version: " ++ (showVersion (Version [0, 2, 1, 2] []))
  | otherwise = interpretFileAndReturn (head args) ((unwords . Util.General.tail') args)
  where
    argsContainAny = any (`elem` args)

interpretFileAndReturn :: FilePath -> String -> IO ()
interpretFileAndReturn filePathToInterpret sakanaArgs = do
  fileHandle <- openFile filePathToInterpret ReadMode
  fileTree <-
    hGetContents fileHandle
      >>= return . generateSyntaxTree
  executeMain
    ((return . getMainEnvironmentStack) fileTree)
    ((return . getMainExecutionTrees) fileTree)
    (return sakanaArgs)
    >>= hPutStrLn stdout . D.fromData
  hClose fileHandle
