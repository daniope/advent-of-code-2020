module Main where

import System.Environment

data Half = 
      Lower
    | Upper
    deriving (Enum, Eq, Ord, Show)

rows = [0..127]
cols = [0..7]

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

half :: Half -> [Int] -> [Int] 
half Lower xs  = take (quot (length xs) 2) xs
half Upper xs  = drop (quot (length xs) 2) xs

loc :: Pass -> [Row] -> Int
loc [] []     = 0  
loc _ [x]     = x
loc (s:ss) xs = loc ss $ half (toHalf s) xs

seat :: Pass -> Seat
seat ss = (loc (take 7 ss) rows, loc (drop 7 ss) cols)

seatID :: Seat -> SeatID
seatID (r, c) = r * 8 + c

readPasses :: FilePath -> IO [Pass]
readPasses = fmap lines . readFile

part1 :: [Pass] -> SeatID
part1 ps = maximum $ map (seatID . seat) ps

main :: IO ()
main = do
    { args <- getArgs
    ; passes <- readPasses $ head args
    ; putStrLn $ "Part 1: " ++ show (part1 passes)
    }
