module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

type ID = Int
type Loc = (Int, Int)
type Direction = (Int, Int)

data Part = 
      Part1
    | Part2
    deriving (Show)

data Status = 
      Floor
    | Empty
    | Occupied
    deriving (Eq, Ord, Show)

type MapSize = (Int, Int)
type SeatMap = Map Loc Status
type ZoneMap = Map Loc [Loc]

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

getAdjacent :: SeatMap -> MapSize -> Loc -> [Loc]
getAdjacent sm size (row, col) = do
    { let status = sm Map.! (row, col)
    ; case status of
        Floor -> []
        _     -> 
            [ (r, c)
            | r <- [row-1..row+1]
            , c <- [col-1..col+1]
            , r >= 0 && r < fst size
            , c >= 0 && c < snd size
            , (r, c) /= (row, col)
            ]
    }

getDirection :: SeatMap -> MapSize -> Loc -> Direction -> Maybe Loc
getDirection sm (rows, cols) (i, j) (h, v) = do
    { if (ni >= 0 && ni < rows) && (nj >= 0 && nj < cols)
        then case sm Map.! (ni, nj) of
            Floor -> getDirection sm (rows, cols) (ni, nj) (h, v)
            _ -> Just (ni, nj)
        else Nothing
    }
    where (ni, nj) = (i + h, j + v)

getVisible :: SeatMap -> MapSize -> Loc -> [Loc]
getVisible sm size (row, col) = do
    { let status = sm Map.! (row, col)
    ; case status of
        Floor -> []
        _     -> do
            { let ls = [ (x, y) | x <- [-1,0,1], y <- [-1,0,1], (x, y) /= (0, 0) ]
            ; let ds = map (\x -> getDirection sm size (row, col) x) ls
            ; [ d | Just d <- ds]
            }
    }
            
seatMap :: [String] -> MapSize -> SeatMap
seatMap ss size = sm
    where ls = getLocs size
          makeSeat x = (x, toStatus $ (ss !! fst x) !! (snd x))
          sm = Map.fromList $ map makeSeat ls

adjacentMap :: SeatMap -> MapSize -> ZoneMap
adjacentMap sm size = zm
    where makeAdjacent key _ = getAdjacent sm size key
          zm = Map.mapWithKey makeAdjacent sm

visibleMap :: SeatMap -> MapSize -> ZoneMap
visibleMap sm size = zm
    where makeVisible key _ = getVisible sm size key
          zm = Map.mapWithKey makeVisible sm

update :: SeatMap -> ZoneMap -> Int -> Loc -> Status
update sm zm limit i = next
    where status = sm Map.! i
          ocs = filter (==Occupied) [ sm Map.! adj | adj <- zm Map.! i ]
          next = case status of
                Floor -> status
                Empty | null ocs -> Occupied
                Occupied | length ocs >= limit -> Empty
                _ -> status

run :: Part -> SeatMap -> ZoneMap -> SeatMap
run part sm zm = Map.mapWithKey f sm
    where limit = case part of
            Part1 -> 4
            Part2 -> 5
          f key _ = update sm zm limit key 

solve :: Part -> SeatMap -> ZoneMap -> Int
solve part sm zm = if (nsm == sm)
    then Map.size $ Map.filter (== Occupied) nsm
    else (solve part nsm zm)
    where nsm = run part sm zm

main :: IO ()
main = do
    { args <-  getArgs
    ; content <- readLines $ head args
    ; let size = getSize content
    ; let sm = seatMap content size
    ; let am = adjacentMap sm size
    ; let vm = visibleMap sm size
    ; putStrLn $ "Part 1: " ++ show (solve Part1 sm am)
    ; putStrLn $ "Part 2: " ++ show (solve Part2 sm vm)
    }
