{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module ToF where

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
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.sum

tof3DTimeVelocityConstraint ::
  (Int, Int, Int)
  -> (Expression Zero R, Expression Three R, Expression Three R, Expression Three R,Expression Three R)
tof3DTimeVelocityConstraint size
 =
  let vx = var3d size "vx"
      vy = var3d size "vy"
      vz = var3d size "vz"
      t = var3d size "t"
      -- up/down neighbours in each slice
      tup = rotate (0, -1, 0) t
      vyup = rotate (0, -1, 0) vy
      vxup = rotate (0, -1, 0) vx
      vzup = rotate (0, -1, 0) vz
      vyuphalf = const 0.5 *. (vy + vyup)
      vxuphalf = const 0.5 *. (vx + vxup)
      vzuphalf = const 0.5 *. (vz + vzup)
      vMatchesTud =
                  (tup - t) * (vxuphalf * vxuphalf + vyuphalf * vyuphalf + vzuphalf * vzuphalf) - vxuphalf

      -- left/right neighbours
      tright = rotate (-1, 0, 0) t
      vxright = rotate (-1, 0, 0) vx
      vyright = rotate (-1, 0 , 0) vy
      vzright = rotate (-1, 0 , 0) vz
      vxrighthalf = const 0.5 *. (vx + vxright)
      vyrighthalf = const 0.5 *. (vy + vyright)
      vzrighthalf = const 0.5 *. (vz + vzright)
      vMatchesTrl =
                  (tright - t) *
                  (vxrighthalf * vxrighthalf + vyrighthalf * vyrighthalf + vzrighthalf * vzrighthalf) -
                  vyrighthalf

      -- up/down slices
      tups = rotate (0, 0, -1) t
      vxups = rotate (0, 0, -1) vx
      vyups = rotate (0, 0 , -1) vy
      vzups = rotate (0, 0 , -1) vz
      vxupshalf = const 0.5 *. (vx + vxups)
      vyupshalf = const 0.5 *. (vy + vyups)
      vzupshalf = const 0.5 *. (vz + vzups)
      vMatchesTusl =
                        (tups - t) *
                        (vxupshalf * vxupshalf + vyupshalf * vyupshalf + vzupshalf * vzupshalf) -
                        vzupshalf
  in (vMatchesTud <.> vMatchesTud + vMatchesTrl <.> vMatchesTrl + vMatchesTusl <.> vMatchesTusl, vx, vy,vz, t)


tof3DUp1 :: (Int, Int, Int) -> [String]
tof3DUp1 size@(sx, sy, sz) =
    let mask = var3d size "mask"
        (vMatchesT, vx, vy,vz , t) = tof3DTimeVelocityConstraint size
        vars = Set.fromList ["t"]
        valMaps =
            emptyVms |>
            withVm3
                (fromList
                     [ ("vx", listArray ((0, 0, 0), (sx - 1, sy - 1, sz-1)) $ repeat 0)
                     , ("vy", listArray ((0, 0, 0), (sx - 1, sy - 1, sz-1)) $ repeat 0)
                     , ("vz", listArray ((0, 0, 0), (sx - 1, sy - 1, sz-1)) $ repeat 1)
                     , ( "mask"
                       , listArray ((0, 0, 0), (sx - 1, sy - 1, sz -1)) $
                         (replicate sz 1) ++ repeat 0)
                     ])
        tZeroOnBottom = mask <.> (t * t * t)
        problem = constructProblem (vMatchesT + tZeroOnBottom) vars
     in generateProblemCode valMaps problem


tof2DTimeVelocityConstraint ::
       (Int, Int)
    -> (Expression Zero R, Expression Two R, Expression Two R, Expression Two R)
tof2DTimeVelocityConstraint size
        -- velocity n-dim array of vectors of the same dimension
        -- 2d means it is a physics experiment
        -- 3d is real flow
        -- we don't have vector variables, so we need multiple variables
 =
    let vx = var2d size "vx"
        vy = var2d size "vy"
        t = var2d size "t"
        -- up/down neighbours
        tup = rotate (0, -1) t
        vyup = rotate (0, -1) vy
        vxup = rotate (0, -1) vx
        vyuphalf = const 0.5 *. (vy + vyup)
        vxuphalf = const 0.5 *. (vx + vxup)
        vMatchesTud =
            (tup - t) * (vxuphalf * vxuphalf + vyuphalf * vyuphalf) - vxuphalf
        -- left/right neighbours
        tright = rotate (-1, 0) t
        vxright = rotate (-1, 0) vx
        vyright = rotate (-1, 0) vy
        vxrighthalf = const 0.5 *. (vx + vxright)
        vyrighthalf = const 0.5 *. (vy + vyright)
        vMatchesTrl =
            (tright - t) *
            (vxrighthalf * vxrighthalf + vyrighthalf * vyrighthalf) -
            vyrighthalf
     in (vMatchesTud <.> vMatchesTud + vMatchesTrl <.> vMatchesTrl, vx, vy, t)

tof2DUp1 :: (Int, Int) -> [String]
tof2DUp1 size@(sx, sy) =
    let mask = var2d size "mask"
        (vMatchesT, vx, vy, t) = tof2DTimeVelocityConstraint size
        vars = Set.fromList ["t"]
        valMaps =
            emptyVms |>
            withVm2
                (fromList
                     [ ("vx", listArray ((0, 0), (sx - 1, sy - 1)) $ repeat 0)
                     , ("vy", listArray ((0, 0), (sx - 1, sy - 1)) $ repeat 1)
                     , ( "mask"
                       , listArray ((0, 0), (sx - 1, sy - 1)) $
                         (replicate sy 1) ++ repeat 0)
                     ])
        tZeroOnBottom = mask <.> (t * t)
        problem = constructProblem (vMatchesT + tZeroOnBottom) vars
     in generateProblemCode valMaps problem
