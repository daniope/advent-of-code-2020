module Main where

import Data.List
import System.IO

parseInts :: [String] -> [Int]
parseInts xs = map read xs

readNumbers :: FilePath -> IO [Int]
readNumbers = (fmap parseInts . (fmap lines . readFile))

getPairs :: [Int] -> Int -> [(Int, Int)]
getPairs xs year = [(x, y) | (x:ys) <- tails xs, y <- ys, x + y == year]

mul :: (Int, Int) -> Int
mul (x, y) = x * y

solve :: [Int] -> Int -> [Int]
solve xs year = map mul (getPairs xs year)

main :: IO ()
main = do
    numbers <- readNumbers "input.txt"
    let result = solve numbers 2020
    putStr "Solution: "
    print result
