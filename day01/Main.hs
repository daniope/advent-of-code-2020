module Main where

import Data.List

getPairs :: [Integer] -> [(Integer, Integer)]
getPairs [] = []
getPairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

main = do
    putStrLn "Pairs:"
    print(getPairs [1, 2, 3, 4, 5])
