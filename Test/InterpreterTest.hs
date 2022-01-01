import Parser.Syntax
import Test.Core
import Util.Tree
import Interpreter.Main
import Parser.Core


main = defaultMain $ testCase "" (assertEqual "bruh" True True)
