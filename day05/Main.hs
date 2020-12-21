module Main where

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
