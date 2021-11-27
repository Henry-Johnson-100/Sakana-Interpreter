import Data.Version
import SakanaParser
import System.Environment
import System.IO
import qualified Token.Data as D
import TreeInterpreter
import qualified Util.General

sakanaVersion :: Version
sakanaVersion = Version [0, 2, 2, 3] []

data LicenseStr = LS String String

instance Show LicenseStr where
  show (LS title copyright) = title ++ "\n\t" ++ copyright

-- | Yes I know this is silly
licensesUsed :: [LicenseStr]
licensesUsed =
  [ LS
      "tasty: Modern and extensible testing framework vs. 0.7 - 1.4.1"
      "Copyright (c) 2013 Roman Cheplyaka",
    LS
      "tasty-hunit: HUnit support for the Tasty test framework ^>=0.10.0.3"
      "Copyright (c) 2013 Roman Cheplyaka\
      \\n\tHUnit is Copyright (c) Dean Herington, 2002, all rights reserved",
    LS
      "parsec: Monadic parser combinators v 3.1.14.0"
      "Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.",
    LS
      "unordered-containers: Efficient hashing-based container types v 0.2.14.0"
      "Copyright (c) 2010, Johan Tibell",
    LS
      "hashable: A class for types that can be converted to a hash value v 1.3.3.0"
      "Copyright Milan Straka 2010"
  ]

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
    mapM_ print licensesUsed
  | otherwise = interpretFileAndReturn (head args) ((unwords . Util.General.tail') args)
  where
    argsContainAny = any (`elem` args)

interpretFileAndReturn :: FilePath -> String -> IO ()
interpretFileAndReturn filePathToInterpret sakanaArgs = do
  fileHandle <- openFile filePathToInterpret ReadMode
  fileTree <-
    hGetContents fileHandle
      >>= return . generateSyntaxTree
  executeMain (getMainRuntime fileTree) (return sakanaArgs)
    >>= hPutStrLn stdout . D.fromData
  hClose fileHandle
