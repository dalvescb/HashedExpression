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
    let x = var "x"
        y = var "y"
        z = exp(x + sin(y))
    print $ diff ["x"] z
