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
            applicativeLaws,
            monadLaws
        ]

applicativeLaws = testGroup "Applicative laws" testList where
    testList =
        [
            applicative_identity,
            applicative_homomorphism,
            applicative_interchange,
            applicative_composition,
            additional_composition
        ]

applicative_identity = testCase name assertion where
    name      = "The identity property of applicatives"
    assertion = assertEqual d a f
    d         = "pure id <*> v == v"
    a         = NBase [1,2,3]
    f         = pure id <*> (NBase [1,2,3])

applicative_homomorphism = testCase name assertion where
    name      = "The homomorphism property of applicatives"
    assertion = assertEqual d a f
    d         = "pure f <*> pure x == pure (f x)"
    a         = pure ((1+) 1) :: Nested Int
    f         = (pure (1+)) <*> pure (1) :: Nested Int

applicative_interchange = testCase name assertion where
    name      = "The interchange property of applicatives"
    assertion = assertEqual d a f
    d         = "u <*> pure y == pure ($ y) <*> u"
    a         = (pure ($ 1)) <*> (NBase [(1+)])
    f         = (NBase [(1+)]) <*> pure 1 :: Nested Int

applicative_composition = testCase name assertion where
    name      = "The composition property of applicatives"
    assertion = assertEqual d a f
    d         = "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
    a         = (NBase [(1+)]) <*> ((NBase [(1+)]) <*> (NBase [1]))
    f         = pure (.) <*> (NBase [(1+)]) <*> (NBase [(1+)]) <*> (NBase [1])

additional_composition = testCase name assertion where
    name      = "Another test case with more complex Nested data"
    assertion = assertEqual d a f
    d         = "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
    a         = (Nest (NBase [(1+)])) <*> ((NBase [(1+)]) <*> (Nest (Nest (NBase [1]))))
    f         = pure (.) <*> (Nest (NBase [(1+)])) <*> (NBase [(1+)]) <*> (Nest (Nest (NBase [1])))

monadLaws = testGroup "Monad laws" testList where
    testList = 
        [
            monad_left_identity,
            monad_right_identity,
            monad_associativity
        ]

monad_left_identity = testCase name assertion where
    name      = "The left identity monad law"
    assertion = assertEqual d a f
    d         = "return a >>= h == h a"
    a         = NBase [2]
    f         = return 1 >>= (\x -> NBase ((1+x):[]))

monad_right_identity = testCase name assertion where
    name      = "The right identity monad law"
    assertion = assertEqual d a f
    d         = "m >>= return == m"
    a         = NBase [1 :: Int]
    f         = (NBase [1 :: Int]) >>= return

monad_associativity = testCase name assertion where
    name      = "The associativity monad law"
    assertion = assertEqual d a f
    d         = "(m >>= g) >>= h == m >>= (\\x -> g x >>= h)"
    a         = (NBase [1]) >>= (\x -> (NBase ((1 + x) : [])) >>= (\z -> NBase ((1+z):[])))
    f         = (NBase [1] >>= (\x -> NBase ((1+x):[]))) >>= (\x -> NBase ((1+x):[]))