{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
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
    , tan
    , tanh
    )

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import HashedCollect
import HashedToC (generateProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.sum

--
--main = do
--    measureTime $ do
--        let exp1 = (((((n +: l)) ^ 3)) ^ 3)
--        let exp2 = ((k +: u) + (p +: j))
--        showExp $ simplify $ exp1 * exp2
main
--    let exp = x * (x1 * y1 * (s *. z1)) <.> sin x1 * z
 = do
    let exp = sum1 [y, l, e]
--    let haha = (s)*((z*((x1*y1*z1)<.>sin(t1)))*x)
--    let exp = x *. y
    showExpDebug . collectDifferentials . exteriorDerivative allVars $ exp
