import Test.Tasty
import Test.Tasty.HUnit
import Data.List

import Token.Util.NestedCollapsible as NC

main = do
    defaultMain tests

tests = testGroup "Token.Util.NestedCollapsible Tests" testList where
    testList =
        [
            instanceVerifyTests
        ]

instanceVerifyTests = testGroup "Instance law verification tests" testList where
    testList =
        [
            applicativeLaws
            --monadLaws
        ]

applicativeLaws = testGroup "Applicative laws" testList where
    testList =
        [
            applicative_identity,
            applicative_homomorphism,
            applicative_interchange,
            applicative_composition
        ]

applicative_identity = testCase name assertion where
    name      = "The identity property of applicatives"
    assertion = assertEqual d a f
    d         = "pure id <*> v == v"
    a         = NBase [1,2,3]
    f         = pure id <*> (NBase [1,2,3])

applicative_homomorphism = testCase name assertion w
    name      = "The homomorphism property of applicatives"
    assertion = assertEqual d a f
    d         = "pure f <*> pure x == pure (f x)"
    a         = pure ((1+) 1) :: Nested Int
    f         = (pure (1+) :: Nested Int) <*> pure (1) :: Nested Int

applicative_interchange = testCase name assertion where
    name      = "The interchange property of applicatives"
    assertion = assertEqual d a f
    d         = "u <*> pure y == pure ($ y) <*> u"
    a         = (pure ($ 1) :: Nested Int) <*> (NBase [(1+)])
    f         = (NBase [(1+)]) <*> pure 1 :: Nested Int

applicative_composition = testCase name assertion where
    name      = "The composition property of applicatives"
    assertion = assertEqual d a f
    d         = "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
    a         = (NBase [(1+)]) <*> ((NBase [(1+)]) <*> (NBase [(1+)]))
    f         = pure (.) <*> (NBase [(1+)]) <*> (NBase [(1+)]) <*> (NBase [(1+)])

{-
monadLaws = testGroup "Monad laws" testList where
    testList = 
        [
        
        ]
        -}