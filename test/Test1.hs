module Test1 where

import Commons
import Data.Maybe (fromJust)
import HashedExpression
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
spec =
    describe "Simplify spec" $
    specify "simplify scalar one zero" $ -- Zero Dimension cases
     do
        x `shouldBe` x
        simplify (const 1 / x) `shouldBe` simplify (x ^ (-1))
        simplify (x + x) `shouldBe` const 2 *. x
        simplify (x - x) `shouldBe` const 0
        simplify (x * x) `shouldBe` simplify (x ^ 2)
        simplify (x / x) `shouldBe` const 1
        simplify (x + y) `shouldBe` simplify (x + y)
        simplify (x - y) `shouldBe` simplify (x - y)
        simplify (x * y) `shouldBe` simplify (x * y)
        simplify (x / y) `shouldBe` simplify (x * y ^ (-1))
        simplify ((x + y) * (x + y)) `shouldBe`
            simplify ((const 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2))
        simplify ((x + y) ^ 2) `shouldBe`
            simplify ((const 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2))
        prettify (simplify ((x - y) ^ 2)) `shouldBe`
            prettify (simplify ((const (-2.0) *. (x * y)) + (x ^ 2) + (y ^ 2)))
        simplify ((x - y) * (x - y)) `shouldBe`
            simplify ((const (-2.0) *. (x * y)) + (x ^ 2) + (y ^ 2))
        simplify ((x * y) ^ 2) `shouldBe` simplify ((x ^ 2) * (y ^ 2))
        simplify ((x * y) * (x * y)) `shouldBe` simplify ((x ^ 2) * (y ^ 2))
        simplify ((x / y) * (x / y)) `shouldBe` simplify ((y ^ (-2)) * (x ^ 2))
        simplify ((x / y) ^ 2) `shouldBe` simplify ((y ^ (-2)) * (x ^ 2))
        simplify ((const 1 / x) * (const 1 / x)) `shouldBe` simplify (x ^ (-2))
        simplify ((const 1 / x) ^ 2) `shouldBe` simplify (x ^ (-2))
        simplify (x * x) `shouldBe` simplify (x ^ 2)
        simplify ((x * x) * x) `shouldBe` simplify (x ^ 3)
        simplify (const 1 / x) `shouldBe` simplify (x ^ (-1))
        simplify (x * x / x) `shouldBe` simplify x
        simplify (x / x) `shouldBe` const 1
        simplify (x / x / x) `shouldBe` simplify (x ^ (-1)) --Expected output
        simplify (x / x / y) `shouldBe` simplify (y ^ (-1)) --((x*(x^-1))*(y^-1))
        simplify ((x * y) ^ 3) `shouldBe` simplify ((x ^ 3) * (y ^ 3))
        simplify (((x * y) ^ 3) * x / y) `shouldBe` simplify ((y ^ 2) * (x ^ 4))
        simplify (((x * y) ^ 3) * x + y) `shouldBe`
            simplify (y + ((y ^ 3) * (x ^ 4))) --Without Paranthesis
        simplify (((x * y) ^ 3) * (x + y)) `shouldBe`
            simplify (((y ^ 3) * (x ^ 4)) + ((x ^ 3) * (y ^ 4))) -- With paranthesis
        simplify (x / (x ^ 3)) `shouldBe` simplify (x ^ (-2)) --FIXED
        simplify ((x ^ 2) ^ 2) `shouldBe` simplify (x ^ 4)
        simplify ((x + y) * (x + y)) `shouldBe`
            simplify ((const 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2))
        simplify (((x + y) * (x + y)) * (x + y)) `shouldBe`
            simplify
                ((const 3.0 *. (y * (x ^ 2))) + ((const 3.0) *. (x * (y ^ 2))) +
                 (x ^ 3) +
                 (y ^ 3))
--        simplify ((x - y) ^ 2) `shouldBe` simplify (   (const 2.0 *. (x * (negate (y)))) + (x ^ 2) + ((negate (y)) ^ 2))
        simplify (((x * y) ^ 3) * (x + y) ^ 2) `shouldBe`
            simplify
                (((y ^ 3) * (x ^ 5)) + ((x ^ 3) * (y ^ 5)) +
                 (const 2.0 *. ((x ^ 4) * (y ^ 4))))
        simplify ((x + y) ^ 2) `shouldBe`
            simplify ((const 2.0 *. (x * y)) + (x ^ 2) + (y ^ 2))
        simplify (x / (x ^ 2)) `shouldBe` simplify (x ^ (-1))
        simplify (x / (x * x)) `shouldBe` simplify (x ^ (-1)) --with paranthesis
        simplify (x / x * x) `shouldBe` simplify x -- without paranthesis
        simplify (x / (x ^ 3)) `shouldBe` simplify (x ^ (-2)) -- FIXED
        simplify ((x * x) / y) `shouldBe` simplify ((y ^ (-1)) * (x ^ 2))
        simplify ((x ^ 2) ^ 3) `shouldBe` simplify (x ^ 6)
        simplify (((x ^ 3) ^ 3) ^ 2) `shouldBe` simplify (x ^ 18)
        simplify (y * (y ^ 2) ^ 2) `shouldBe` simplify (y ^ 5)
        simplify (x / ((y ^ 2) ^ 3)) `shouldBe` simplify (x * y ^ (-6))
        simplify (((x * y) ^ 2) / x) `shouldBe` simplify (x * y ^ 2)
        prettify (simplify (((x + y) ^ 2) * ((x - y) ^ 2))) `shouldBe`
            prettify
                (simplify
                     ((const (-2.0) *. ((x ^ 2) * (y ^ 2))) + (x ^ 4) + (y ^ 4)))
        simplify (((x + y) ^ 2) + ((x - y) ^ 2)) `shouldBe`
            simplify ((const 2.0 *. (x ^ 2)) + (const 2.0 *. (y ^ 2)))
        simplify (((x + y) ^ 2) - ((x - y) ^ 2)) `shouldBe`
            simplify (const 4.0 *. (x * y))
        simplify ((x + y) * (x - y)) `shouldBe`
            simplify ((const (-1.0) *. (y ^ 2)) + (x ^ 2))
        simplify ((x + y) + (x - y)) `shouldBe` simplify (const 2.0 *. x)
        simplify ((x + y) - (x - y)) `shouldBe` simplify (const 2.0 *. y)
        simplify ((x + y) / (x - y)) `shouldBe`
            simplify
                ((x * ((x + (const (-1.0) *. y)) ^ (-1))) +
                 (y * ((x + (const (-1.0) *. y)) ^ (-1))) --FIXME
                 )
        simplify (x * x) `shouldBe` simplify (x ^ 2)
        simplify (x ^ 0) `shouldBe` const 1.0
        simplify (x ^ 1) `shouldBe` simplify x
--        simplify (const 3*.x)/(const 2*.x) `shouldBe` (const 1 *.x) --FIXME
--DOT PRODUCT
        simplify (x <.> y) `shouldBe` simplify (x * y)
        simplify (x <.> const 1) `shouldBe` simplify x
        simplify (x <.> const (-1)) `shouldBe` simplify (const (-1) * x)
        simplify (x <.> const 0) `shouldBe` const 0
        simplify (x <.> x) `shouldBe` simplify x ^ 2
        simplify (x <.> x * y) `shouldBe` simplify (y * (x ^ 2))
        simplify (x <.> x + x) `shouldBe` simplify (x + (x ^ 2))
        simplify (x <.> (x + x)) `shouldBe` simplify (const 2.0 *. (x ^ 2))
        simplify (x <.> (const 1 / x)) `shouldBe` const 1
        simplify (x <.> (y / x)) `shouldBe` simplify y
        simplify ((x <.> (x + y)) ^ 2) `shouldBe`
            simplify
                (((x ^ 2) * (y ^ 2)) + (const 2.0 *. (y * (x ^ 3))) + (x ^ 4))
       -- simplify (x+x) <.> y `shouldBe` simplify (const 2.0 *.(x*y)) --(2.0*.(x*y))  But error while testing it FIXME
--         simplify (x*.x) <.> x `shouldBe` simplify x^3) -- Error while testing FIXME
--One Dimension TC
        simplify x1 `shouldBe` simplify x1
        simplify (x1 * x1) `shouldBe` simplify (x1 ^ 2)
        simplify (x1 + x1) `shouldBe` simplify (const 2.0 *. x1)
--        simplify (x1 - x1) `shouldBe` const1d 10 0 -- FIXME error for with n without const 0, 0.0, Zero,
--        simplify (x1 / x1) `shouldBe` 1 --FIXME
        simplify (x1 + y1) `shouldBe` simplify ((x1 + y1))
        simplify (x1 - y1) `shouldBe` simplify (x1 + (const (-1.0) *. y1))
        simplify (x1 * y1) `shouldBe` simplify (x1 * y1)
        simplify (x1 / y1) `shouldBe` simplify (x1 * (y1) ^ (-1))
        simplify ((x1 + y1) ^ 2) `shouldBe`
            simplify ((const 2.0 *. (x1 * y1)) + ((x1) ^ 2) + ((y1) ^ 2))
        simplify ((x1 - y1) ^ 2) `shouldBe`
            simplify ((const (-2.0) *. (x1 * y1)) + ((x1) ^ 2) + ((y1) ^ 2))
        simplify ((x1 * y1) ^ 2) `shouldBe` simplify (((x1) ^ 2) * ((y1) ^ 2))
        simplify ((x1 / y1) ^ 2) `shouldBe`
            simplify (((y1) ^ (-2)) * ((x1) ^ 2))
        simplify ((x1 <.> y1) ^ 2) `shouldBe` simplify (((x1 <.> y1)) ^ 2)
        simplify ((const 2 *. x1) ^ 2) `shouldBe`
            simplify (const 4.0 *. ((x1) ^ 2))
        simplify ((const 3 *. x1) / (const 2 *. y1)) `shouldBe`
            simplify (const 3.0 *. (x1 * (((const 2.0 *. y1)) ^ (-1)))) --FIXME
--        simplify (x1^0) `shouldBe` simplify const 1.0 --FIXME
        simplify (x1 ^ 1) `shouldBe` simplify (x1)
        simplify (const 1.0 *. x1) `shouldBe` simplify (x1)
--        simplify (const 1/x1) `shouldBe` simplify (x1^-1) --FIXME
--        simplify (x1 / x1) `shouldBe` const 1 --FIXME
        simplify ((x1 - y1) * (x1 + y1)) `shouldBe`
            simplify
                ((const (-1.0) *. ((y1) ^ 2)) + ((x1) ^ 2))
--        simplify ((x1+y1)/(x1-y1)) `shouldBe` simplify ((x1*(((x1+(const (-1.0)*.y1)))^(-1)))+(y1*(((x1+(const (-1.0)*.y1)))^(-1))))
--  simplify (x ^ x) `shouldBe` simplify x --Not possible


---- All Zero cases to One Dimension cases
        x1 `shouldBe` x1
        simplify (x1 + x1) `shouldBe` const 2 *. x1
        simplify (x1 * x1) `shouldBe` simplify (x1 ^ 2)
        simplify (x1 + y1) `shouldBe` simplify (x1 + y1)
        simplify (x1 - y1) `shouldBe` simplify (x1 - y1)
        simplify (x1 * y1) `shouldBe` simplify (x1 * y1)
        simplify (x1 / y1) `shouldBe` simplify (x1 * y1 ^ (-1))
        simplify ((x1 + y1) * (x1 + y1)) `shouldBe` simplify ((const 2.0 *. (x1 * y1)) + (x1 ^ 2) + (y1 ^ 2))
        simplify ((x1 + y1) ^ 2) `shouldBe` simplify ((const 2.0 *. (x1 * y1)) + (x1 ^ 2) + (y1 ^ 2))
        prettify (simplify ((x1 - y1) ^ 2)) `shouldBe` prettify (simplify ((const (-2.0) *. (x1 * y1)) + (x1 ^ 2) + (y1 ^ 2)))
        simplify ((x1 - y1) * (x1 - y1)) `shouldBe` simplify ((const (-2.0) *. (x1 * y1)) + (x1 ^ 2) + (y1 ^ 2))
        simplify ((x1 * y1) ^ 2) `shouldBe` simplify ((x1 ^ 2) * (y1 ^ 2))
        simplify ((x1 * y1) * (x1 * y1)) `shouldBe` simplify ((x1 ^ 2) * (y1 ^ 2))
        simplify ((x1 / y1) * (x1 / y1)) `shouldBe` simplify ((y1 ^ (-2)) * (x1 ^ 2))
        simplify ((x1 / y1) ^ 2) `shouldBe` simplify ((y1 ^ (-2)) * (x1 ^ 2))
--        simplify ((const 1 / x1) * (const 1 / x1)) `shouldBe` simplify (x1 ^ (-2)) --FIXME
--        simplify ((const 1 / x1) ^ 2) `shouldBe` simplify (x1 ^ (-2)) --FIXME
        simplify (x1 * x1) `shouldBe` simplify (x1 ^ 2)
        simplify ((x1 * x1) * x1) `shouldBe` simplify (x1 ^ 3)
        simplify (x1 * x1 / x1) `shouldBe` simplify x1
        simplify (x1 / x1 / x1) `shouldBe` simplify (x1 ^ (-1))
        simplify (x1 / x1 / y1) `shouldBe` simplify (y1 ^ (-1))
        simplify ((x1 * y1) ^ 3) `shouldBe` simplify ((x1 ^ 3) * (y1 ^ 3))
        simplify (((x1 * y1) ^ 3) * x1 / y1) `shouldBe` simplify ((y1 ^ 2) * (x1 ^ 4))
        simplify (((x1 * y1) ^ 3) * x1 + y1) `shouldBe` simplify (y1 + ((y1 ^ 3) * (x1 ^ 4)))
        simplify (((x1 * y1) ^ 3) * (x1 + y1)) `shouldBe` simplify (((y1 ^ 3) * (x1 ^ 4)) + ((x1 ^ 3) * (y1 ^ 4)))
        simplify (x1 / (x1 ^ 3)) `shouldBe` simplify (x1 ^ (-2))
        simplify ((x1 ^ 2) ^ 2) `shouldBe` simplify (x1 ^ 4)
        simplify ((x1 + y1) * (x1 + y1)) `shouldBe` simplify ((const 2.0 *. (x1 * y1)) + (x1 ^ 2) + (y1 ^ 2))
        simplify  (((x1 + y1) * (x1 + y1)) * (x1 + y1)) `shouldBe` simplify ((const 3.0 *. (y1 * (x1 ^ 2))) + ((const 3.0) *. (x1 * (y1 ^ 2))) + (x1 ^ 3) + (y1 ^ 3))
        simplify ((x1 - y1) ^ 2) `shouldBe` simplify ((const 2.0 *. (x1 * (negate (y1)))) + (x1 ^ 2) + ((negate (y1)) ^ 2))
        simplify (((x1 * y1) ^ 3) * (x1 + y1) ^ 2) `shouldBe` simplify (((y1 ^ 3) * (x1 ^ 5)) + ((x1 ^ 3) * (y1 ^ 5)) + (const 2.0 *. ((x1 ^ 4) * (y1 ^ 4))))
        simplify ((x1 + y1) ^ 2) `shouldBe` simplify ((const 2.0 *. (x1 * y1)) + (x1 ^ 2) + (y1 ^ 2))
        simplify (x1 / (x1 ^ 2)) `shouldBe`  simplify (x1 ^ (-1))
        simplify (x1 / (x1 * x1)) `shouldBe` simplify (x1 ^ (-1))
        simplify (x1 / x1 * x1) `shouldBe` simplify x1
        simplify (x1 / (x1 ^ 3)) `shouldBe` simplify (x1 ^ (-2))
        simplify ((x1 * x1) / y1) `shouldBe` simplify ((y1 ^ (-1)) * (x1 ^ 2))
        simplify ((x1 ^ 2) ^ 3) `shouldBe` simplify (x1 ^ 6)
        simplify (((x1 ^ 3) ^ 3) ^ 2) `shouldBe` simplify (x1 ^ 18)
        simplify (y1 * (y1 ^ 2) ^ 2) `shouldBe` simplify (y1 ^ 5)
        simplify (x1 / ((y1 ^ 2) ^ 3)) `shouldBe` simplify (x1 * y1 ^ (-6))
        simplify (((x1 * y1) ^ 2) / x1) `shouldBe` simplify (x1 * y1 ^ 2)
        prettify (simplify (((x1 + y1) ^ 2) * ((x1 - y1) ^ 2))) `shouldBe` prettify (simplify ((const (-2.0) *. ((x1 ^ 2) * (y1 ^ 2))) + (x1 ^ 4) + (y1 ^ 4)))
        simplify (((x1 + y1) ^ 2) + ((x1 - y1) ^ 2)) `shouldBe` simplify ((const 2.0 *. (x1 ^ 2)) + (const 2.0 *. (y1 ^ 2)))
        simplify (((x1 + y1) ^ 2) - ((x1 - y1) ^ 2)) `shouldBe` simplify (const 4.0 *. (x1 * y1))
        simplify ((x1 + y1) * (x1 - y1)) `shouldBe` simplify ((const (-1.0) *. (y1 ^ 2)) + (x1 ^ 2))
        simplify ((x1 + y1) + (x1 - y1)) `shouldBe` simplify (const 2.0 *. x1)
        simplify ((x1 + y1) - (x1 - y1)) `shouldBe`  simplify (const 2.0 *. y1)
        simplify ((x1 + y1) / (x1 - y1)) `shouldBe` simplify ((x1 * ((x1 + (const (-1.0) *. y1)) ^ (-1))) + (y1 * ((x1 + (const (-1.0) *. y1)) ^ (-1))) )
        simplify (x1 * x1) `shouldBe` simplify (x1 ^ 2)
        --simplify (x1 ^ 0) `shouldBe`  const 1.0 -- FIXME
        simplify (x1 ^ 1) `shouldBe` simplify x1
--       --        simplify (const 3*.x1)/(const 2*.x1) `shouldBe` (const 1 *.x1) --FIx1ME
--       --DOT PRODUCT --FIXME Dot product doesnt work in one dimension
--        simplify  (x1 <.> y1) `shouldBe` simplify (x1 * y1) --FIXME
--        simplify (x1 <.> const 1) `shouldBe` simplify x1
--        simplify (x1 <.> const (-1)) `shouldBe` simplify (const (-1) * x1) --FIXME
--        simplify (x1 <.> const 0) `shouldBe` const 0
--        simplify (x1 <.> x1) `shouldBe` simplify x1 ^ 2 --FIXME
--        simplify (x1 <.> x1 * y1) `shouldBe` simplify (y1 * (x1 ^ 2))
--        simplify (x1 <.> x1 + x1) `shouldBe` simplify (x1 + (x1 ^ 2)) --FIXME
--        simplify (x1 <.> (x1 + x1)) `shouldBe` simplify (const 2.0 *. (x1 ^ 2)) --FIXME
--        simplify (x1 <.> (const 1 / x1)) `shouldBe` const 1 --FIXME
--        simplify (x1 <.> (y1 / x1)) `shouldBe` simplify y1 --FIXME
--        simplify ((x1 <.> (x1 + y1)) ^ 2) `shouldBe` simplify (((x1 ^ 2) * (y1 ^ 2)) + (const 2.0 *. (y1 * (x1 ^ 3))) + (x1 ^ 4)) --FIXME

--Two dimension cases

        x2 `shouldBe` x2
--        simplify (const 1 / x2) `shouldBe` simplify (x2 ^ (-1))
        simplify (x2 + x2) `shouldBe` const 2 *. x2
--        simplify (x2 - x2) `shouldBe` const 0
        simplify (x2 * x2) `shouldBe` simplify (x2 ^ 2)
--        simplify (x2 / x2) `shouldBe` const 1
        simplify (x2 + y2) `shouldBe` simplify (x2 + y2)
        simplify (x2 - y2) `shouldBe` simplify (x2 - y2)
        simplify (x2 * y2) `shouldBe` simplify (x2 * y2)
        simplify (x2 / y2) `shouldBe` simplify (x2 * y2 ^ (-1))
        simplify ((x2 + y2) * (x2 + y2)) `shouldBe`
            simplify ((const 2.0 *. (x2 * y2)) + (x2 ^ 2) + (y2 ^ 2))
        simplify ((x2 + y2) ^ 2) `shouldBe`
            simplify ((const 2.0 *. (x2 * y2)) + (x2 ^ 2) + (y2 ^ 2))
        prettify (simplify ((x2 - y2) ^ 2)) `shouldBe`
            prettify (simplify ((const (-2.0) *. (x2 * y2)) + (x2 ^ 2) + (y2 ^ 2)))
        simplify ((x2 - y2) * (x2 - y2)) `shouldBe`
            simplify ((const (-2.0) *. (x2 * y2)) + (x2 ^ 2) + (y2 ^ 2))
        simplify ((x2 * y2) ^ 2) `shouldBe` simplify ((x2 ^ 2) * (y2 ^ 2))
        simplify ((x2 * y2) * (x2 * y2)) `shouldBe` simplify ((x2 ^ 2) * (y2 ^ 2))
        simplify ((x2 / y2) * (x2 / y2)) `shouldBe` simplify ((y2 ^ (-2)) * (x2 ^ 2))
        simplify ((x2 / y2) ^ 2) `shouldBe` simplify ((y2 ^ (-2)) * (x2 ^ 2))
--        simplify ((const 1 / x2) * (const 1 / x2)) `shouldBe` simplify (x2 ^ (-2))
--        simplify ((const 1 / x2) ^ 2) `shouldBe` simplify (x2 ^ (-2))
        simplify (x2 * x2) `shouldBe` simplify (x2 ^ 2)
        simplify ((x2 * x2) * x2) `shouldBe` simplify (x2 ^ 3)
--        simplify (const 1 / x2) `shouldBe` simplify (x2 ^ (-1))
        simplify (x2 * x2 / x2) `shouldBe` simplify x2
--        simplify (x2 / x2) `shouldBe` const 1
        simplify (x2 / x2 / x2) `shouldBe` simplify (x2 ^ (-1)) --Ex2pected output
        simplify (x2 / x2 / y2) `shouldBe` simplify (y2 ^ (-1)) --((x2*(x2^-1))*(y2^-1))
        simplify ((x2 * y2) ^ 3) `shouldBe` simplify ((x2 ^ 3) * (y2 ^ 3))
        simplify (((x2 * y2) ^ 3) * x2 / y2) `shouldBe` simplify ((y2 ^ 2) * (x2 ^ 4))
        simplify (((x2 * y2) ^ 3) * x2 + y2) `shouldBe`
            simplify (y2 + ((y2 ^ 3) * (x2 ^ 4))) --Without Paranthesis
        simplify (((x2 * y2) ^ 3) * (x2 + y2)) `shouldBe`
            simplify (((y2 ^ 3) * (x2 ^ 4)) + ((x2 ^ 3) * (y2 ^ 4))) -- With paranthesis
        simplify (x2 / (x2 ^ 3)) `shouldBe` simplify (x2 ^ (-2)) --FIx2ED
        simplify ((x2 ^ 2) ^ 2) `shouldBe` simplify (x2 ^ 4)
        simplify ((x2 + y2) * (x2 + y2)) `shouldBe`
            simplify ((const 2.0 *. (x2 * y2)) + (x2 ^ 2) + (y2 ^ 2))
        simplify (((x2 + y2) * (x2 + y2)) * (x2 + y2)) `shouldBe`
            simplify
                ((const 3.0 *. (y2 * (x2 ^ 2))) + ((const 3.0) *. (x2 * (y2 ^ 2))) +
                 (x2 ^ 3) +
                 (y2 ^ 3))
--        simplify ((x2 - y2) ^ 2) `shouldBe` simplify (   (const 2.0 *. (x2 * (negate (y2)))) + (x2 ^ 2) + ((negate (y2)) ^ 2))
        simplify (((x2 * y2) ^ 3) * (x2 + y2) ^ 2) `shouldBe`
            simplify
                (((y2 ^ 3) * (x2 ^ 5)) + ((x2 ^ 3) * (y2 ^ 5)) +
                 (const 2.0 *. ((x2 ^ 4) * (y2 ^ 4))))
        simplify ((x2 + y2) ^ 2) `shouldBe`
            simplify ((const 2.0 *. (x2 * y2)) + (x2 ^ 2) + (y2 ^ 2))
        simplify (x2 / (x2 ^ 2)) `shouldBe` simplify (x2 ^ (-1))
        simplify (x2 / (x2 * x2)) `shouldBe` simplify (x2 ^ (-1)) --with paranthesis
        simplify (x2 / x2 * x2) `shouldBe` simplify x2 -- without paranthesis
        simplify (x2 / (x2 ^ 3)) `shouldBe` simplify (x2 ^ (-2)) -- FIx2ED
        simplify ((x2 * x2) / y2) `shouldBe` simplify ((y2 ^ (-1)) * (x2 ^ 2))
        simplify ((x2 ^ 2) ^ 3) `shouldBe` simplify (x2 ^ 6)
        simplify (((x2 ^ 3) ^ 3) ^ 2) `shouldBe` simplify (x2 ^ 18)
        simplify (y2 * (y2 ^ 2) ^ 2) `shouldBe` simplify (y2 ^ 5)
        simplify (x2 / ((y2 ^ 2) ^ 3)) `shouldBe` simplify (x2 * y2 ^ (-6))
        simplify (((x2 * y2) ^ 2) / x2) `shouldBe` simplify (x2 * y2 ^ 2)
        prettify (simplify (((x2 + y2) ^ 2) * ((x2 - y2) ^ 2))) `shouldBe`
            prettify
                (simplify
                     ((const (-2.0) *. ((x2 ^ 2) * (y2 ^ 2))) + (x2 ^ 4) + (y2 ^ 4)))
        simplify (((x2 + y2) ^ 2) + ((x2 - y2) ^ 2)) `shouldBe`
            simplify ((const 2.0 *. (x2 ^ 2)) + (const 2.0 *. (y2 ^ 2)))
        simplify (((x2 + y2) ^ 2) - ((x2 - y2) ^ 2)) `shouldBe`
            simplify (const 4.0 *. (x2 * y2))
        simplify ((x2 + y2) * (x2 - y2)) `shouldBe`
            simplify ((const (-1.0) *. (y2 ^ 2)) + (x2 ^ 2))
        simplify ((x2 + y2) + (x2 - y2)) `shouldBe` simplify (const 2.0 *. x2)
        simplify ((x2 + y2) - (x2 - y2)) `shouldBe` simplify (const 2.0 *. y2)
        simplify ((x2 + y2) / (x2 - y2)) `shouldBe`
            simplify
                ((x2 * ((x2 + (const (-1.0) *. y2)) ^ (-1))) +
                 (y2 * ((x2 + (const (-1.0) *. y2)) ^ (-1))) --FIXME
                 )
        simplify (x2 * x2) `shouldBe` simplify (x2 ^ 2)
--        simplify (x2 ^ 0) `shouldBe` const 1.0
        simplify (x2 ^ 1) `shouldBe` simplify x2
--        simplify (const 3*.x2)/(const 2*.x2) `shouldBe` (const 1 *.x2) --FIXME

----DOT PRODUCT
--        simplify (x2 <.> y2) `shouldBe` simplify (x2 * y2)
--        simplify (x2 <.> const 1) `shouldBe` simplify x2
--        simplify (x2 <.> const (-1)) `shouldBe` simplify (const (-1) * x2)
--        simplify (x2 <.> const 0) `shouldBe` const 0
--        simplify (x2 <.> x2) `shouldBe` simplify x2 ^ 2
--        simplify (x2 <.> x2 * y2) `shouldBe` simplify (y2 * (x2 ^ 2))
--        simplify (x2 <.> x2 + x2) `shouldBe` simplify (x2 + (x2 ^ 2))
--        simplify (x2 <.> (x2 + x2)) `shouldBe` simplify (const 2.0 *. (x2 ^ 2))
----        simplify (x2 <.> (const 1 / x2)) `shouldBe` const 1
--        simplify (x2 <.> (y2 / x2)) `shouldBe` simplify y2
--        simplify ((x2 <.> (x2 + y2)) ^ 2) `shouldBe`
--            simplify
--                (((x2 ^ 2) * (y2 ^ 2)) + (const 2.0 *. (y2 * (x2 ^ 3))) + (x2 ^ 4))


--        specify "simplify scalar one zero" $ do
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
--        specify "simplify log and exponential" $ do
        simplify (log (exp x)) `shouldBe` x
        simplify (exp (log x)) `shouldBe` x
--        specify "complex related" $ do
        prettify (simplify ((x +: y) * (z +: w))) `shouldBe`
                prettify (simplify ((x * z - y * w) +: (x * w + y * z)))
        simplify (xRe (x +: y)) `shouldBe` x
        simplify (xIm (x +: y)) `shouldBe` y
        simplify ((x +: y) + (u +: v)) `shouldBe`
                simplify ((x + u) +: (y + v))
        simplify (s *. (x +: y)) `shouldBe` simplify ((s *. x) +: (s *. y))
        simplify ((x +: y) * (z +: w)) `shouldBe`
                simplify ((x * z - y * w) +: (x * w + y * z))
--        specify "dot product" $ do
        simplify (x <.> zero) `shouldBe` zero
        simplify (zero <.> x) `shouldBe` zero
        prettify (simplify ((s *. x) <.> y)) `shouldBe`
                prettify (simplify (s *. (x <.> y)))
        simplify (x <.> (s *. y)) `shouldBe` simplify (s *. (x <.> y))
--        specify "distributivity" $ do
        simplify (x * (y + z)) `shouldBe` (x * y + x * z)
        simplify ((y + z) * x) `shouldBe` (x * y + x * z)
        (simplify (x *. (y + z))) `shouldBe` (simplify (x *. y + x *. z))
        prettify (simplify (simplify (x <.> (y + z)))) `shouldBe`
                prettify (simplify ((x <.> y) + (x <.> z)))
        simplify ((y + z) <.> x) `shouldBe` simplify ((y <.> x) + (z <.> x))
        simplify (x * sum [y, z, t, u, v]) `shouldBe`
                simplify (sum (map (x *) [y, z, t, u, v]))
        simplify (sum [y, z, t, u, v] * x) `shouldBe`
                simplify (sum (map (x *) [y, z, t, u, v]))
        simplify (x *. sum [y, z, t, u, v]) `shouldBe`
                simplify (sum (map (x *.) [y, z, t, u, v]))
        simplify (x <.> sum [y, z, t, u, v]) `shouldBe`
                simplify (sum (map (x <.>) [y, z, t, u, v]))
        simplify (sum [y, z, t, u, v] <.> x) `shouldBe`
                simplify (sum (map (<.> x) [y, z, t, u, v]))
        prettify (simplify (product [a, b, c, sum [x, y, z]])) `shouldBe`
                prettify
                    (simplify (sum (map (product . (: [a, b, c])) [x, y, z])))
        simplify ((x + y) * (z + t) * a * b) `shouldBe`
                simplify
                    (a * b * x * z + a * b * x * t + a * b * y * z +
                     a * b * y * t)
--        specify "flatten sum and product" $ do
        simplify (product [x * y, product [z, t, w], one]) `shouldBe`
                simplify (product [x, y, z, t, w])
        simplify (sum [x + y, sum [z, t, w + s], zero]) `shouldBe`
                simplify (sum [x, y, z, t, w, s])
--        specify "group constants together" $ do
        simplify (product [one, one, x, y, one, z]) `shouldBe`
                product [x, y, z]
        prettify (simplify (sum [one, one, x, y, one, z])) `shouldBe`
                prettify (simplify (sum [const 3, x, y, z]))
        simplify (product [const 1, const 2, x, y, const 3, z]) `shouldBe`
                simplify (product [const 6, x, y, z])
--        specify "combine same terms" $ do
        prettify (simplify (sum [one *. x1, x1, x1, const 3 *. y1, y1])) `shouldBe`
                prettify (simplify (sum [const 3 *. x1, const 4 *. y1]))
        simplify (sum [const (-1) *. x1, x1, const 3 *. y1, y1, z1]) `shouldBe`
                simplify (sum [const 4 *. y1, z1])
        simplify (x1 - x1) `shouldBe` zero1
        prettify (simplify (sum [one *. x, x, x, const 3 *. y, y])) `shouldBe`
                prettify (simplify (sum [const 3 *. x, const 4 *. y]))
        simplify (sum [const (-1) *. x, x, const 3 *. y, y, z]) `shouldBe`
                simplify (sum [const 4 *. y, z])
        simplify (x - x) `shouldBe` zero
--        specify "scale rules" $ do
        simplify (x *. (y *. v)) `shouldBe` simplify ((x * y) *. v)
        simplify (xRe (x *. xc)) `shouldBe` simplify (x *. xRe xc)
        simplify (xIm (x *. xc)) `shouldBe` simplify (x *. xIm xc)
--        specify "negate rules" $ do
        simplify (negate (negate x)) `shouldBe` simplify x
        prettify (simplify (negate (negate (x + y)))) `shouldBe`
                prettify (simplify (x + y))
        simplify (negate zero) `shouldBe` zero
