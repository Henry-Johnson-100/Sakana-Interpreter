module Main where

import qualified Token.Util.EagerCollapsible as EagerCollapsible
    ( dropInfix )

main :: IO ()
main = putStrLn $ concat $ EagerCollapsible.dropInfix ["2", "3"] ["1", "2", "3", "4"]
