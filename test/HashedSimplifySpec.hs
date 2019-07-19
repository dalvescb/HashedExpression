module HashedSimplifySpec where

import Commons
import Data.Complex (Complex(..))
import Data.Maybe (fromJust)
import HashedExpression
import HashedInterp ((~=))
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
import HashedVar
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
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
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , sum
    , tan
    , tanh
    )
import Test.Hspec

spec :: Spec
spec = do
    describe "Simplify spec" $ do
        specify "simplify scalar one zero" $ do
            simplify (const 0.0 *. const 9.0) `shouldBe` const 0.0
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
            prettify (simplify ((x * y) * one)) `shouldBe` prettify (x * y)
            simplify (x * y * z * one) `shouldBe` simplify (x * y * z)
            simplify (product [x, y, z, t, w, zero]) `shouldBe` zero
        specify "simplify log and exponential" $ do
            simplify (log (exp x)) `shouldBe` x
            simplify (exp (log x)) `shouldBe` x
        specify "complex related" $ do
            prettify (simplify ((x +: y) * (z +: w))) `shouldBe`
                prettify (simplify ((x * z - y * w) +: (x * w + y * z)))
            simplify (xRe (x +: y)) `shouldBe` x
            simplify (xIm (x +: y)) `shouldBe` y
            simplify ((x +: y) + (u +: v)) `shouldBe`
                simplify ((x + u) +: (y + v))
            simplify (s *. (x +: y)) `shouldBe` simplify ((s *. x) +: (s *. y))
            simplify ((x +: y) * (z +: w)) `shouldBe`
                simplify ((x * z - y * w) +: (x * w + y * z))
        specify "dot product" $ do
            simplify (x <.> zero) `shouldBe` zero
            simplify (zero <.> x) `shouldBe` zero
            prettify (simplify ((s *. x) <.> y)) `shouldBe`
                prettify (simplify (s * (x <.> y)))
            simplify (x <.> (s *. y)) `shouldBe` simplify (s * (x <.> y))
        specify "distributivity" $ do
            simplify (x * (y + z)) `shouldBe` (x * y + x * z)
            simplify ((y + z) * x) `shouldBe` (x * y + x * z)
            (simplify (x *. (y + z))) `shouldBe` (simplify (x *. y + x *. z))
            prettify (simplify (simplify (x <.> (y + z)))) `shouldBe`
                prettify (simplify ((x <.> y) + (x <.> z)))
            simplify ((y + z) <.> x) `shouldBe` simplify ((x <.> y) + (x <.> z))
            simplify (x * sum [y, z, t, u, v]) `shouldBe`
                simplify (sum (map (x *) [y, z, t, u, v]))
            simplify (sum [y, z, t, u, v] * x) `shouldBe`
                simplify (sum (map (x *) [y, z, t, u, v]))
            simplify (x *. sum [y, z, t, u, v]) `shouldBe`
                simplify (sum (map (x *.) [y, z, t, u, v]))
            simplify (x <.> sum [y, z, t, u, v]) `shouldBe`
                simplify (sum (map (x <.>) [y, z, t, u, v]))
            simplify (sum [y, z, t, u, v] <.> x) `shouldBe`
                simplify (sum (map (x <.>) [y, z, t, u, v]))
            prettify (simplify (product [a, b, c, sum [x, y, z]])) `shouldBe`
                prettify
                    (simplify (sum (map (product . (: [a, b, c])) [x, y, z])))
            simplify ((x + y) * (z + t) * a * b) `shouldBe`
                simplify
                    (a * b * x * z + a * b * x * t + a * b * y * z +
                     a * b * y * t)
        specify "flatten sum and product" $ do
            simplify (product [x * y, product [z, t, w], one]) `shouldBe`
                simplify (product [x, y, z, t, w])
            simplify (sum [x + y, sum [z, t, w + s], zero]) `shouldBe`
                simplify (sum [x, y, z, t, w, s])
        specify "group constants together" $ do
            simplify (product [one, one, x, y, one, z]) `shouldBe`
                product [x, y, z]
            prettify (simplify (sum [one, one, x, y, one, z])) `shouldBe`
                prettify (simplify (sum [const 3, x, y, z]))
            simplify (product [const 1, const 2, x, y, const 3, z]) `shouldBe`
                simplify (product [const 6, x, y, z])
        specify "combine same terms" $
            -- Higher dimension this is correct
         do
            prettify (simplify (sum [one *. x1, x1, x1, const 3 *. y1, y1])) `shouldBe`
                prettify (simplify (sum [const 3 *. x1, const 4 *. y1]))
            simplify (sum [const (-1) *. x1, x1, const 3 *. y1, y1, z1]) `shouldBe`
                simplify (sum [const 4 *. y1, z1])
            simplify (x1 - x1) `shouldBe` zero1 One Dimesion 
            prettify (simplify (sum [one *. x, x, x, const 3 *. y, y])) `shouldBe`
                prettify (simplify (sum [const 3 *. x, const 4 *. y]))
            simplify (sum [const (-1) *. x, x, const 3 *. y, y, z]) `shouldBe`
                simplify (sum [const 4 *. y, z])
            simplify (x - x) `shouldBe` zero
        specify "scale rules" $ do
            simplify (x *. (y *. v)) `shouldBe` simplify ((x * y) *. v)
            simplify (xRe (x *. xc)) `shouldBe` simplify (x *. xRe xc)
            simplify (xIm (x *. xc)) `shouldBe` simplify (x *. xIm xc)
        specify "negate rules" $ do
            simplify (negate (negate x)) `shouldBe` simplify x
            prettify (simplify (negate (negate (x + y)))) `shouldBe`
                prettify (simplify (x + y))
            simplify (negate zero) `shouldBe` zero
    describe "Simplify spec higher dimension" $ do
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
        specify "dot product higher dimension with scaling and point wise" $ do
            simplify (x1 <.> zero1) `shouldBe` zero
            simplify (zero1 <.> x1) `shouldBe` zero
            simplify ((s *. x1) <.> y1) `shouldBe` simplify (s * (x1 <.> y1))
            simplify (x1 <.> (s *. y1)) `shouldBe` simplify (s * (x1 <.> y1))
            simplify (x1 * (y1 + z1)) `shouldBe` simplify (x1 * y1 + x1 * z1)
            simplify ((y1 + z1) * x1) `shouldBe` simplify (x1 * y1 + x1 * z1)
            simplify (s *. (y1 + z1)) `shouldBe` simplify (s *. y1 + s *. z1)
            simplify (x1 <.> (y1 + z1)) `shouldBe`
                simplify ((x1 <.> y1) + (x1 <.> z1))
            simplify ((y1 + z1) <.> x1) `shouldBe`
                simplify ((x1 <.> y1) + (x1 <.> z1))
        specify "log and exp higher" $ do
            simplify (log (exp x1)) `shouldBe` x1
            simplify (exp (log x1)) `shouldBe` x1
            simplify (log (exp x2)) `shouldBe` x2
            simplify (exp (log x2)) `shouldBe` x2

--Higher Dimensions
    describe "Simplify One Dimension spec" $ do
        specify "simplify  One Dimesion  one zero" $ do
            simplify (const 0.0 *. const 9.0) `shouldBe` const 0.0 --Check if const could be used with array
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
            prettify (simplify ((x1 * y1) * one1)) `shouldBe` prettify (x1 * y1)
            simplify (x1 * y1 * z1 * one1) `shouldBe` simplify (x1 * y1 * z1)
            simplify (product [x1, y1, z1, t1, w1, zero1]) `shouldBe` zero1
        specify "simplify One Dimesion log and exponential" $ do
            simplify (log (exp x1)) `shouldBe` x1
            simplify (exp (log x1)) `shouldBe` x1
        specify "complex One Dimesion  related" $ do
            prettify (simplify ((x1 +: y1) * (z1 +: w1))) `shouldBe`
                prettify (simplify ((x1 * z1 - y1 * w1) +: (x1 * w1 + y1 * z1)))
            simplify (xRe (x1 +: y1)) `shouldBe` x1
            simplify (xIm (x1 +: y1)) `shouldBe` y1
            simplify ((x1 +: y1) + (u1 +: v1)) `shouldBe`
                simplify ((x1 + u1) +: (y1 + v1))
            simplify (s *. (x1 +: y1)) `shouldBe` simplify ((s *. x1) +: (s *. y1))
            simplify ((x1 +: y1) * (z1 +: w1)) `shouldBe`
                simplify ((x1 * z1 - y1 * w1) +: (x1 * w1 + y1 * z1))
        specify " One Dimesion dot product" $ do
            simplify (x1 <.> zero1) `shouldBe` zero
            simplify (zero1 <.> x1) `shouldBe` zero
            prettify (simplify ((s *. x1) <.> y1)) `shouldBe`
                prettify (simplify (s * (x1 <.> y1)))
            simplify (x1 <.> (s *. y1)) `shouldBe` simplify (s * (x1 <.> y1))
        specify " One Dimesion distributivity" $ do
            simplify (x1 * (y1 + z1)) `shouldBe` (x1 * z1 + x1 * y1)
            simplify ((y1 + z1) * x1) `shouldBe` (x1 * z1 + x1 * y1)
            (simplify (x *. (y1 + z1))) `shouldBe` (simplify (x *. z1 + x *. y1))
            prettify (simplify (simplify (x1 <.> (y1 + z1)))) `shouldBe`
                prettify (simplify ((x1 <.> y1) + (x1 <.> z1)))
            simplify ((y1 + z1) <.> x1) `shouldBe` simplify ((x1 <.> y1) + (x1 <.> z1))
            simplify (x1 * sum [y1, z1, t1, u1, v1]) `shouldBe`
                simplify (sum (map (x1 *) [y1, z1, t1, u1, v1]))
            simplify (sum [y1, z1, t1, u1, v1] * x1) `shouldBe`
                simplify (sum (map (x1 *) [y1, z1, t1, u1, v1]))
            simplify (x *. sum [y1, z1, t1, u1, v1]) `shouldBe`
                simplify (sum (map (x *.) [y1, z1, t1, u1, v1]))
            simplify (x1 <.> sum [y1, z1, t1, u1, v1]) `shouldBe`
                simplify (sum (map (x1 <.>) [y1, z1, t1, u1, v1]))
            simplify (sum [y1, z1, t1, u1, v1] <.> x1) `shouldBe`
                simplify (sum (map (x1 <.>) [y1, z1, t1, u1, v1]))
            prettify (simplify (product [a1, b1, c1, sum [x1, y1, z1]])) `shouldBe`
                prettify
                    (simplify (sum (map (product . (: [a1, b1, c1])) [x1, y1, z1])))
            simplify ((x1 + y1) * (z1 + t1) * a1 * b1) `shouldBe`
                simplify
                    (a1 * b1 * x1 * z1 + a1 * b1 * x1 * t1 + a1 * b1 * y1 * z1 +
                     a1 * b1 * y1 * t1)
        specify " One Dimesion flatten sum and product" $ do
            simplify (product [x1 * y1, product [z1, t1, w1], one1]) `shouldBe`
                simplify (product [x1, y1, z1, t1, w1])
            simplify (sum [x1 + y1, sum [z1, t1, w1 + s1], zero1]) `shouldBe`
                simplify (sum [x1, y1, z1, t1, w1, s1])
        specify " One Dimesion group constants together" $ do
            simplify (product [one1, one1, x1, y1, one1, z1]) `shouldBe`
                product [x1, y1, z1]
            prettify (simplify (sum [one1, one1, x1, y1, one1, z1])) `shouldBe`
                prettify (simplify (sum [(const1d 10) 3, x1, y1, z1]))
            simplify (product [(const1d 10) 1, (const1d 10) 2, x1, y1, (const1d 10) 3, z1]) `shouldBe`
                simplify (product [(const1d 10) 6, x1, y1, z1])
        specify " One Dimesion combine same terms" $
            -- Higher dimension this is correct
         do
            prettify (simplify (sum [one *. x1, x1, x1, const 3 *. y1, y1])) `shouldBe`
                prettify (simplify (sum [const 3 *. x1, const 4 *. y1]))
            simplify (sum [const (-1) *. x1, x1, const 3 *. y1, y1, z1]) `shouldBe`
                simplify (sum [const 4 *. y1, z1])
            simplify (x1 - x1) `shouldBe` zero1
            prettify (simplify (sum [one *. x1, x1, x1, const 3 *. y1, y1])) `shouldBe`
                prettify (simplify (sum [const 3 *. x1, const 4 *. y1]))
            simplify (sum [const (-1) *. x1, x1, const 3 *. y1, y1, z1]) `shouldBe`
                simplify (sum [const 4 *. y1, z1])
            simplify (x1 - x1) `shouldBe` zero1
        specify " One Dimesion scale rules" $ do
            simplify (x *. (y *. v1)) `shouldBe` simplify ((x * y) *. v1)
            simplify (xRe (x *. xc)) `shouldBe` simplify (x *. xRe xc)
            simplify (xIm (x *. xc)) `shouldBe` simplify (x *. xIm xc)
        specify " One Dimesion negate rules" $ do
            simplify (negate (negate x1)) `shouldBe` simplify x1
            prettify (simplify (negate (negate (x1 + y1)))) `shouldBe`
                prettify (simplify (x1 + y1))
            simplify (negate zero1) `shouldBe` zero1