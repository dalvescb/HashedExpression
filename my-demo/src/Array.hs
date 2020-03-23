module Array where

import Data.Array
import Control.Monad.ST
import Data.Array.ST
main = do
    let a = array (0, 4) [(i, 0) | i <- [0..4]]
    putStrLn $ "emp: " ++ show a

    let a' = a // [(4,100)]
    putStrLn $ "set: " ++ show a'
    putStrLn $ "get: " ++ show (a' ! 4)
    putStrLn $ "len: " ++ show ((+1) . snd . bounds $ a')

    let b = array (0, 4) [(i, i+1) | i <- [0..4]]
    putStrLn $ "dcl: " ++ show b

    let twoD = array ((0,0), (1, 2)) [((i, j), i + j) | i <- [0..1], j <- [0..2]]
    putStrLn $ "2d: " ++ show twoD
    print $ runST buildPair

buildPair = do arr <- newArray (1,10) 37 :: ST s (STArray s Int Int)
               a <- readArray arr 1
               writeArray arr 1 64
               b <- readArray arr 1
               return (a,b)