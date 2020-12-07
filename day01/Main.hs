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

solve :: [Int] -> [Int]
solve xs = map mul (getPairs xs 2020)

main :: IO ()
main = do
    numbers <- readNumbers "input.txt"
    let result = solve numbers
    putStr "Solution: "
    print result
