module Fib where

factorial :: Int -> Int
factorial n
  | n <=0 = 1
  | n >0 = n * factorial ( n-1 )

main :: IO()
main = do
        let x = factorial 5
        print (x)
