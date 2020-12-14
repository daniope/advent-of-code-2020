module Main where

import Data.Tuple
import System.Environment

data Element =
      Empty
    | Tree
    deriving (Enum, Eq, Show)

parse_element :: Char -> Element
parse_element el = case el of
    '.' -> Empty
    '#' -> Tree

get_element :: [String] -> (Int,Int) -> Element
get_element s coord = do
    { let el = (s !! (fst coord)) !! (snd coord)
    ; parse_element el
    }

get_coords :: [String] -> Int -> Int -> [(Int, Int)]
get_coords s right down = map swap $ zip [0,right..] [0,down..length s - 1]

get_elements :: [String] -> Int -> Int -> [Element]
get_elements s right down = 
    [ get_element s (row, (rem col $ length $ head s))
    | (row, col) <- get_coords s right down
    ]

get_trees :: [Element] -> [Element]
get_trees els = filter (== Tree) els

solve_1 :: [String] -> Int -> Int -> Int
solve_1 ss right down = do
    { let els = get_elements ss right down
    ; let trees = get_trees els
    ; length trees
    }

solve_2 :: [String] -> [(Int,Int)] -> Int
solve_2 ss slopes = do
    { product $ map (\x -> solve_1 ss (fst x) (snd x)) slopes
    }

main :: IO ()
main = do
    { args <- getArgs
    ; content <- fmap lines $ readFile $ head args
    ; putStr "Part 1: "
    ; print $ solve_1 content 3 1 
    ; putStr "Part 2: "
    ; print $ solve_2 content [(1,1), (3,1), (5,1), (7,1), (1,2)]
    }
