module SimplifySpec where

import HashedExpression
import HashedOperation
import HashedPrettify
import HashedSimplify
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import Test.Hspec
import TestCommons

spec :: Spec
spec = do
    describe "Simplify spec" $ do
        specify "simplify scalar one zero" $ do
            simplify (x * one) `shouldBe` x
            simplify (one * x) `shouldBe` x
            simplify (x * zero) `shouldBe` zero
            simplify (zero * x) `shouldBe` zero
            simplify (y * (x * zero)) `shouldBe` zero
            simplify (zero * (x * one)) `shouldBe` zero
            simplify (zero * x * one) `shouldBe` zero
            simplify (zero * (x * y)) `shouldBe` zero
            simplify ((x * y) * zero) `shouldBe` zero
            simplify ((x * zero) * one) `shouldBe` zero
            simplify ((x * y) * one) `shouldBe` (x * y)
            simplify (x * y * z * one) `shouldBe` simplify (x * y * z)
        specify "simplify one d one zero" $ do
            simplify (x1 * one1) `shouldBe` x1
            simplify (one1 * x1) `shouldBe` x1
            simplify (x1 * zero1) `shouldBe` zero1
            simplify (zero1 * x1) `shouldBe` zero1
            simplify (y1 * (x1 * zero1)) `shouldBe` zero1
            simplify (zero1 * (x1 * one1)) `shouldBe` zero1
            simplify (zero1 * x1 * one1) `shouldBe` zero1
            simplify (zero1 * (x1 * y1)) `shouldBe` zero1
            simplify ((x1 * y1) * zero1) `shouldBe` zero1
            simplify ((x1 * zero1) * one1) `shouldBe` zero1
            simplify ((x1 * y1) * one1) `shouldBe` (x1 * y1)
            simplify (x1 * y1 * z1 * one1) `shouldBe` simplify (x1 * y1 * z1)
        specify "simplify log and exponential" $ do
            simplify (log (exp (x))) `shouldBe` x
            simplify (exp (log (x))) `shouldBe` x
            simplify (log (exp (x1))) `shouldBe` x1
            simplify (exp (log (x1))) `shouldBe` x1
            simplify (log (exp (x2))) `shouldBe` x2
            simplify (exp (log (x2))) `shouldBe` x2
        specify "complex related" $ do
            simplify ((x +: y) * (z +: w)) `shouldBe` (x * z - y * w) +:
                (x * w + y * z)
            simplify (xRe (x +: y)) `shouldBe` x
            simplify (xIm (x +: y)) `shouldBe` y
            simplify ((x +: y) + (u +: v)) `shouldBe` (x + u) +: (y + v)
            simplify (s *. (x +: y)) `shouldBe` (s *. x) +: (s *. y) -- does not work for ScalarC, only vectorC; it's also in HashedComplexInstances
            simplify ((x +: y) * (z +: w)) `shouldBe` (x * z - y * w) +:
                (x * w + y * z)
        specify "dot product" $ do
            simplify (x <.> zero) `shouldBe` zero
            simplify (zero <.> x) `shouldBe` zero
            simplify ((s *. x) <.> y) `shouldBe` s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
            simplify (x <.> (s *. y)) `shouldBe` s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
            simplify (x * (y + z)) `shouldBe` (x * y + x * z)
            simplify ((y + z) * x) `shouldBe` (x * y + x * z)
            simplify (x *. (y + z)) `shouldBe` (x *. y + x *. z)
            simplify ((x <.> (y + z))) `shouldBe` ((x <.> y) + (x <.> z))
            simplify (((y + z) <.> x)) `shouldBe` ((x <.> y) + (x <.> z))
        specify "dot product higher dimension with scaling and point wise" $ do
            simplify (x1 <.> zero1) `shouldBe` zero
            simplify (zero1 <.> x1) `shouldBe` zero
            simplify ((s *. x1) <.> y1) `shouldBe` s * (x1 <.> y1)
            simplify (x1 <.> (s *. y1)) `shouldBe` s * (x1 <.> y1)
            simplify (x1 * (y1 + z1)) `shouldBe` (x1 * y1 + x1 * z1) -- TODO: Why????
            simplify ((y1 + z1) * x1) `shouldBe` (x1 * y1 + x1 * z1)
            simplify (s *. (y1 + z1)) `shouldBe` (s *. y1 + s *. z1)
            simplify ((x1 <.> (y1 + z1))) `shouldBe` ((x1 <.> y1) + (x1 <.> z1))
            simplify (((y1 + z1) <.> x1)) `shouldBe` ((x1 <.> y1) + (x1 <.> z1))