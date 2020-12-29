module Main where

import Data.List
import System.Environment

type Preamble = Int

parse :: FilePath -> IO [Int]
parse = fmap rd . (fmap lines . readFile)
    where rd = map read

valid :: [Int] -> Int -> Bool
valid xs n = length pairs > 0
    where pairs = [ [x, y] | (x:ys) <- tails xs, y <- ys, x + y == n]

section :: [Int] -> Int -> Int -> [Int]
section xs i n = (take n . drop i) xs

solve1 :: [Int] -> Preamble -> [Int]
solve1 xs p =
    [ x
    | (x, i) <- zip (drop p xs) [p..]
    , not $ valid (section xs (i - p) p) x
    ]

main :: IO ()
main = do
    { args <- getArgs
    ; xs <- parse $ head args
    ; let preamble = read $ args !! 1
    ; putStrLn $ show (head $ solve1 xs preamble)
    }
