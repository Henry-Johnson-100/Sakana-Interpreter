module Main where

import Data.Version (showVersion)
import Lexer (tokenize)
import Paths_Sakana (version)
import SyntaxTree (generateSyntaxTree)
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
    putStrLn $ "Sakana Interpeter Version: " ++ (showVersion Paths_Sakana.version)
  | otherwise = interpretFileAndReturn (head args)
  where
    argsContainAny = any (`elem` args)

interpretFileAndReturn :: FilePath -> IO ()
interpretFileAndReturn filePathToInterpret = do
  fileHandle <- openFile filePathToInterpret ReadMode
  fileTree <-
    hGetContents fileHandle
      >>= return . generateSyntaxTree . tokenize
  executeMain
    ((return . getMainEnvironmentStack) fileTree)
    ((return . getMainExecutionTrees) fileTree)
    >>= hPutStrLn stdout . D.fromData
  hClose fileHandle
