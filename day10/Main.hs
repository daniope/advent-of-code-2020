module Main where

import Data.List
import System.Environment

parse :: FilePath -> IO [Int]
parse = fmap rd . (fmap lines . readFile)
    where rd = map read

intervals :: [Int] -> [Int]
intervals (x:[]) = []
intervals (x:xs) = head xs - x : intervals xs

solve1 :: [Int] -> Int
solve1 xs = (length $ filter (==1) ys) * ((length $ filter (==3) ys) + 1)
    where ys = intervals $ sort $ 0:xs

main :: IO ()
main = do
    { args <- getArgs
    ; xs <- parse $ head args
    ; putStrLn $ "Part 1: " ++ show (solve1 xs)
    }
