module Main where

import Data.List
import System.Environment

data Half = 
      Lower
    | Upper
    deriving (Show)

type Row = Int
type Col = Int

type Pass = String
type Seat = (Row, Col)
type SeatID = Int

toHalf :: Char -> Half
toHalf 'F' = Lower
toHalf 'B' = Upper
toHalf 'L' = Lower
toHalf 'R' = Upper

half :: Half -> (Int, Int) -> (Int, Int) 
half Lower (l, h) = (l, l + (div (h - l) 2))
half Upper (l, h) = (l + (div (h - l) 2) + 1, h)

loc :: Pass -> (Int, Int) -> Int
loc [] x     = fst x  
loc (s:ss) x = loc ss $ half (toHalf s) x

seat :: Pass -> Seat
seat ss = (loc (take 7 ss) (0, 127), loc (drop 7 ss) (0, 7))

seatID :: Seat -> SeatID
seatID (r, c) = r * 8 + c

seatIDs :: [Pass] -> [SeatID]
seatIDs ps = map (seatID . seat) ps

passes :: FilePath -> IO [Pass]
passes = fmap lines . readFile

part1 :: [Pass] -> SeatID
part1 ps = maximum $ seatIDs ps

part2 :: [Pass] -> SeatID
part2 ps = do
    { let ids = seatIDs ps
    ; head [ x | x <- [minimum ids..maximum ids], not $ elem x ids ]
    }

main :: IO ()
main = do
    { args <- getArgs
    ; ps <- passes $ head args
    ; putStrLn $ "Part 1: " ++ show (part1 ps)
    ; putStrLn $ "Part 2: " ++ show (part2 ps)
    }
