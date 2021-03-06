module Main where

import Data.List
import System.Environment

type Preamble = Int

parse :: FilePath -> IO [Int]
parse = fmap rd . (fmap lines . readFile)
    where rd = map read

valid :: [Int] -> Int -> Bool
valid xs n = not $ null pairs
    where pairs = [ [x, y] | (x:ys) <- tails xs, y <- ys, x + y == n]

section :: [Int] -> Int -> Int -> [Int]
section xs i n = (take n . drop i) xs

solve1 :: [Int] -> Preamble -> Int
solve1 xs p = head ys
    where ys =
            [ x
            | (x, i) <- zip (drop p xs) [p..]
            , not $ valid (section xs (i - p) p) x
            ]

helper :: [Int] -> Int -> [Int] -> Int
helper xs n (y:ys) = if null ss
    then helper xs n ys
    else minimum s + maximum s
        where s = head ss
              ss =
                [ sec
                | (z, i) <- zip xs [0..(length xs)-y]
                , let sec = section xs i y
                , sum sec == n
                ]

solve2 :: [Int] -> Int -> Int
solve2 xs n = helper xs n [2..length xs]

main :: IO ()
main = do
    { args <- getArgs
    ; xs <- parse $ head args
    ; let preamble = read $ args !! 1
    ; let n = solve1 xs preamble
    ; putStrLn $ "Part 1: " ++ show n
    ; putStrLn $ "Part 2: " ++ show (solve2 xs n)
    }
