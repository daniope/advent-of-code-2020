module Main where

import Data.List
import System.Environment

parseInts :: [String] -> [Int]
parseInts xs = map read xs

readNumbers :: FilePath -> IO [Int]
readNumbers = fmap parseInts . (fmap lines . readFile)

getPairs :: [Int] -> Int -> [[Int]]
getPairs xs year = [[x, y] | (x:ys) <- tails xs, y <- ys, x + y == year]

getTrios :: [Int] -> Int -> [[Int]]
getTrios xs year = [
    [x, y, z]
    | (x:ys) <- tails xs
    , (y:zs) <- tails ys 
    , z <- zs
    , x + y + z == year
    ]

prod :: [Int] -> Int
prod []     = 1
prod (x:xs) = x * prod xs

solve :: [Int] -> Int -> Int -> [Int]
solve xs 2 year = map prod (getPairs xs year)
solve xs 3 year = map prod (getTrios xs year)
solve xs _ year = []

main :: IO ()
main = do
    args <- getArgs
    numbers <- readNumbers $ head args
    putStr "Part 1: "
    print $ solve numbers 2 2020
    putStr "Part 2: "
    print $ solve numbers 3 2020
