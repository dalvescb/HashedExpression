{-# LANGUAGE DataKinds #-}
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
import ToF.ToF

import Data.List (intercalate)
import Data.Maybe (fromJust)
import GHC.TypeLits
import Data.STRef.Strict
import HashedCollect
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import ToF.VelocityGenerator

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, NumType et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.product

--main = do
--    let (problem, valMaps, (vX, vY)) = tof2DQuarterCircle (50, 50) 30 10 0.15
--    putStrLn $ unwords . map show . elems $ vX
--    putStrLn $ unwords . map show . elems $ vY
--    let code = generateProblemCode valMaps problem
--    let filePath = "algorithms/lbfgs/problem.c"
--    writeFile filePath $ intercalate "\n" code
--main = do
--    let (vX, vY) = quarterCircleFlow (20, 20) 7 6 0.15
--main = do
--    let exp = huber 1 x2 <.> one2
--    let vars = Set.fromList ["x2"]
--    showExp $ collectDifferentials . exteriorDerivative vars $ exp
--    let valMaps = fromList []
--    let problem = constructProblem exp vars
--    let codes = generateProblemCode valMaps problem
--    writeFile "algorithms/lbfgs/problem.c" $ intercalate "\n" codes--main = do
--    let exp = piecewise [1, 2, 3] x [y, z, t, x]
--    let valMaps =
--            fromList
--                [ ("x", VScalar 1)
--                , ("y", VScalar 1)
--                , ("z", VScalar 2)
--                , ("t", VScalar 3)
--                ]
--    let program = singleExpressionCProgram valMaps exp
--    let fileName = "haha"
--    let fullFileName = "C/" ++ fileName ++ ".c"
--    let program = singleExpressionCProgram valMaps exp
--    writeFile fullFileName (intercalate "\n" program)
main = do 
    let varX = var1d' "x" :: Expression 10 R
    let varY = var1d' "y" :: Expression 10 R
    let varZ = var2d' "z" :: Expression '(10, 10) R
    let one2d = const2d' 0 :: Expression '(10, 10) R
    
--    let exp = varZ <.> varZ
    return ()
