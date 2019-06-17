{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array.Unboxed as U
import HashedDerivative
import HashedExpression
import HashedFactor
import HashedInstances
import HashedInterp
import HashedSimplify

import Test.Hspec
import Test.QuickCheck hiding (scale)

-- TODO run tests? or anything really
main = do
    let x = var1d 10 "x"
        y = var1d 10 "y"
--        z = exp ( 1 * x + sin y)
        z = (x + y) + (y + x)
    print $ z
    print $ simplify z
