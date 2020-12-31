module Main where

import Data.List
import System.Environment

parse :: FilePath -> IO [Int]
parse = fmap rd . (fmap lines . readFile)
    where rd = map read

factorial :: Int -> Int
factorial x = product [1..x]

intervals :: [Int] -> [Int]
intervals (x:[]) = []
intervals (x:xs) = head xs - x : intervals xs

combinations :: [Int] -> [Int]
combinations []     = []
combinations xs = c : (combinations $ drop (n + 1) xs)
    where ys = [ y | y <- takeWhile (==1) $ xs ]
          n  = length ys
          c  = case n of
            0 -> 1
            _ -> (2 ^ (n - 1)) - fromEnum (n == 4) 

solve1 :: [Int] -> Int
solve1 xs = (count 1) * ((count 3) + 1)
    where ys = intervals $ sort $ 0:xs
          count y = length $ filter (==y) ys

solve2 :: [Int] -> Int
solve2 xs = product cs
    where ys = intervals $ sort $ 0:xs
          cs = combinations ys

main :: IO ()
main = do
    { args <- getArgs
    ; xs <- parse $ head args
    ; putStrLn $ "Part 1: " ++ show (solve1 xs)
    ; putStrLn $ "Part 2: " ++ show (solve2 xs)
    }
