module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

type ID = Int
type Loc = (Int, Int)

data Status = 
      Floor
    | Empty
    | Occupied
    deriving (Eq, Ord, Show)

type SeatMap = Map Loc Status
type AdjacentMap = Map Loc [Loc]
type MapSize = (Int, Int)

toStatus :: Char -> Status
toStatus '.' = Floor
toStatus 'L' = Empty
toStatus '#' = Occupied
toStatus _   = error "Invalid status"

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

getSize :: [String] -> MapSize
getSize ss = (length ss, length $ head ss)

getLocs :: MapSize -> [Loc]
getLocs (rows, cols) = [ (i , j) | i <- [0..rows-1], j <- [0..cols-1]]

getAdjacent :: MapSize -> Loc -> [Loc]
getAdjacent size (row, col) = 
    [ (r, c)
    | r <- [row-1..row+1]
    , c <- [col-1..col+1]
    , r >= 0 && r < fst size
    , c >= 0 && c < snd size
    , (r, c) /= (row, col)
    ]

seatMap :: [String] -> MapSize -> SeatMap
seatMap ss size = sm
    where ls = getLocs size
          makeSeat x = (x, toStatus $ (ss !! fst x) !! (snd x))
          sm = Map.fromList $ map makeSeat ls

adjacentMap :: SeatMap -> MapSize -> AdjacentMap
adjacentMap sm size = am
    where makeAdjacent key _ = getAdjacent size key
          am = Map.mapWithKey makeAdjacent sm

update :: SeatMap -> AdjacentMap -> Loc -> Status
update sm am i = next
    where status = sm Map.! i
          ocs = filter (==Occupied) [ sm Map.! adj | adj <- am Map.! i ]
          next = case status of
                Empty | null ocs -> Occupied
                Occupied | length ocs >= 4 -> Empty
                _ -> status

run :: SeatMap -> AdjacentMap -> SeatMap
run sm am = Map.mapWithKey f sm
    where f key x = update sm am key 

solve1 :: SeatMap -> AdjacentMap -> Int
solve1 sm am = if (nsm == sm)
    then Map.size $ Map.filter (== Occupied) nsm
    else (solve1 nsm am)
    where nsm = run sm am

main :: IO ()
main = do
    { args <-  getArgs
    ; content <- readLines $ head args
    ; let size = getSize content
    ; let sm = seatMap content size
    ; let am = adjacentMap sm size
    ; putStrLn $ "Part 1: " ++ show (solve1 sm am)
    }
