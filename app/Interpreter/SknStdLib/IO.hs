{-# LANGUAGE MagicHash #-}

module Interpreter.SknStdLib.IO
  ( exporting,
  )
where

import qualified Data.Maybe as Maybe
import qualified Exception.Base as Exception
import qualified Interpreter.Inspection
import qualified Interpreter.SknStdLib.Type as StdLibType
import qualified Syntax
import System.IO (hGetContents, hPutStr, stderr, stdin, stdout)
import qualified Util.Classes as UC
import qualified Util.General as UGen
import Util.Tree (Tree ((:-<-:)))
import qualified Util.Tree as Tree

exporting :: [StdLibType.SknStdLibFunction]
exporting = [trout, herring, dolphin]

trout :: StdLibType.SknStdLibFunction
trout = StdLibType.GeneralStdLibFunction "trout" trout_params# trout#
  where
    trout_params# = ["to_print"]
    trout# trs = trout'# trs
      where
        trout'#
          (((Syntax.SyntaxUnit (Syntax.Data (Syntax.String str)) _ _) :-<-: []) : []) =
            hPutStr stdout str >> return UC.defaultValue
        trout'# (tr : []) = (Exception.raiseError . printTypeException) tr
        trout'# trs =
          StdLibType.raiseSknStdLibArgumentException (trs) ("") (trout_params#)

herring :: StdLibType.SknStdLibFunction
herring = StdLibType.GeneralStdLibFunction "herring" herring_params# herring#
  where
    herring_params# = ["to_print"]
    herring# trs = herring'# trs
      where
        herring'#
          (((Syntax.SyntaxUnit (Syntax.Data (Syntax.String str)) _ _) :-<-: []) : []) =
            hPutStr stderr str >> return UC.defaultValue
        herring'# (tr : []) = (Exception.raiseError . printTypeException) tr
        herring'# trs =
          StdLibType.raiseSknStdLibArgumentException (trs) ("") (herring_params#)

dolphin :: StdLibType.SknStdLibFunction
dolphin = StdLibType.GeneralStdLibFunction dolphin_id# dolphin_params# dolphin#
  where
    dolphin_id# = "dolphin"
    dolphin_params# = []
    dolphin# trs = dolphin'# trs
      where
        dolphin'# [] = do
          inString <- hGetContents stdin
          let stringDataTree =
                Tree.tree
                  UC.defaultValue {Syntax.token = Syntax.Data (Syntax.String inString)}
          return stringDataTree
        dolphin'# trs =
          StdLibType.raiseSknStdLibArgumentException
            (trs)
            ("This function takes no arguments!")
            (dolphin_params#)

printTypeException :: Syntax.SyntaxTree -> Exception.Exception
printTypeException tr =
  Exception.newException
    Exception.TypeException
    ( ( map Syntax.line
          . Tree.flattenTree
      )
        tr
    )
    ("Error printing tree: " ++ UC.format tr ++ "\n Not a string primitive type.")
    Exception.Fatal