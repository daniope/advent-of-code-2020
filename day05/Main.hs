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

getHalf :: Half -> [Int] -> [Int] 
getHalf Lower xs  = take (quot (length xs) 2) xs
getHalf Upper xs  = drop (quot (length xs) 2) xs

getRow :: Pass -> [Row] -> Int
getRow [] []     = 0  
getRow _ [x]    = x
getRow (s:ss) xs = getRow ss $ getHalf (toHalf s) xs

getCol :: Pass -> [Col] -> Int
getCol [] []     = 0  
getCol _ [x]    = x
getCol (s:ss) xs = getCol ss $ getHalf (toHalf s) xs

getSeat :: Pass -> Seat
getSeat ss = (getRow (take 7 ss) rows, getCol (drop 7 ss) cols)

getSeatID :: Pass -> SeatID
getSeatID ss = do
    { let seat = getSeat ss
    ; (fst seat * 8) + (snd seat)
    }

readPasses :: FilePath -> IO [Pass]
readPasses = fmap lines . readFile

main :: IO ()
main = do
    { args <- getArgs
    ; passes <- readPasses $ head args
    ; putStrLn $ "Part 1: " ++ show (maximum $ map getSeatID passes)
    }
