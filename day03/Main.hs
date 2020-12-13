module Main where

import System.Environment

data Element =
      Empty
    | Tree
    deriving (Enum, Eq, Show)

parse_element :: Char -> Element
parse_element el = case el of
    '.' -> Empty
    '#' -> Tree

get_element :: [String] -> Int -> Int -> Element
get_element s row col = do
    { let el = (s !! row) !! col
    ; parse_element el
    }

get_elements :: [String] -> Int -> Int -> [Element]
get_elements s right down = 
    [ get_element s row (rem col $ length $ s !! 0)
    | (col, row) <- zip [0,right..] [0,down..length s - 1]
    ]

get_trees :: [Element] -> [Element]
get_trees els = filter (== Tree) els

solve :: [String] -> Int -> Int -> Int
solve ss down right = do
    { let els = get_elements ss down right
    ; let trees = get_trees els
    ; length trees
    }

main :: IO ()
main = do
    { args <- getArgs
    ; content <- fmap lines $ readFile $ head args
    ; putStr "Part 1: "
    ; print $ solve content 3 1 
    }
