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

sakanaVersion :: Version
sakanaVersion = Version [0, 3, 0, 0] []

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
      \\t --licenses\n\
      \\t\tDisplays additional licenses of other software\
      \ libraries appearing in this code. See the root_directory/LibrariesUsed.md\
      \ for more information.\n\
      \\t-h, --help\n\
      \\t\tDisplays this help."
  | argsContainAny ["-v", "--version"] =
    putStrLn $ "Sakana Interpeter Version: " ++ (showVersion sakanaVersion)
  | argsContainAny ["--licenses"] = do
    putStrLn
      "parsec: Monadic parser combinators v 3.1.14.0\n\
      \\tCopyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved."
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
