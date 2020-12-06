module Main where

import Data.List

mul :: (Integer, Integer) -> Integer
mul (x, y) = x * y

getPairs :: [Integer] -> Integer -> [(Integer, Integer)]
getPairs [] _ = []
getPairs l year = [(x, y) | (x:ys) <- tails l, y <- ys, x + y == year]

solve :: [Integer] -> [Integer]
solve l = map mul (getPairs l 2020)

main :: IO ()
main = do
    print(solve [1721, 979, 366, 299, 675, 1456])
