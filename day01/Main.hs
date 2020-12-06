module Main where

import Data.List

mul :: (Integer, Integer) -> Integer
mul (x, y) = x * y

getPairs :: [Integer] -> [(Integer, Integer)]
getPairs [] = []
getPairs l = [(x, y) | (x:ys) <- tails l, y <- ys, x + y == 2020]

solve :: [Integer] -> [Integer]
solve l = map mul (getPairs l)

main :: IO ()
main = do
    print(solve [1721, 979, 366, 299, 675, 1456])
