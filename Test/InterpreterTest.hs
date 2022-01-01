import Data.Either
import Data.Maybe
import Interpreter.Main
import Parser.Main
import Parser.Syntax
import System.IO.Unsafe (unsafePerformIO)
import Test.Core
import Util.Classes
import Util.Tree

expectData :: Either SyntaxTree Data -> Data
expectData = either (error "Expected data but got a SyntaxTree.") id

expectTree :: Either SyntaxTree Data -> SyntaxTree
expectTree = either id (error "Expected a SyntaxTree but got Data.")

asNum :: Data -> Double
asNum d = fromMaybe (error ("Expecting a Num but got " ++ format d)) (unNum d)

asBool :: Data -> Bool
asBool d = fromMaybe (error ("Expecting a Boolean but got " ++ format d)) (unBoolean d)

asString :: Data -> String
asString d = fromMaybe (error ("Expecting a String but got " ++ format d)) (unString d)

evaluateString :: String -> Either SyntaxTree Data
evaluateString = unsafePerformIO . flip evaluateProgram [] . parse "TestString"

main = defaultMain $ testCase "" (assertEqual "bruh" True True)
