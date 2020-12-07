module Main where

import Data.List
import System.IO

--readInts :: String -> IO [Integer]
--readInts file = map read (lines file)

readInts :: [String] -> [Integer]
readInts xs = map read xs

readNumbers :: FilePath -> IO [Integer]
readNumbers file = (fmap readInts . (fmap lines . readFile)) file

getPairs :: [Integer] -> Integer -> [(Integer, Integer)]
getPairs xs year = [(x, y) | (x:ys) <- tails xs, y <- ys, x + y == year]

mul :: (Integer, Integer) -> Integer
mul (x, y) = x * y

solve :: [Integer] -> [Integer]
solve xs = map mul (getPairs xs 2020)

main :: IO ()
main = do
    numbers <- readNumbers "result.txt"
    let result = solve numbers
    putStr "Solution: "
    print result
