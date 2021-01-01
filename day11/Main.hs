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

parse :: FilePath -> IO [[Status]]
parse = fmap parse . (fmap lines . readFile)
    where parse ss = [ map toStatus st | st <- ss ]

getSize :: [[Status]] -> MapSize
getSize sts = (length sts, length $ head sts)

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

maps :: [[Status]] -> (AdjacentMap, SeatMap)
maps ss = (am, sm)
    where size = getSize ss
          ls = [ (i , j) | i <- [0..(fst size)-1], j <- [0..(snd size)-1]]
          makeAdjacent x = (x, getAdjacent size x)
          makeSeat x = (x, (ss !! fst x) !! (snd x))
          am = Map.fromList (map makeAdjacent ls)
          sm = Map.fromList (map makeSeat ls)

update :: (AdjacentMap, SeatMap) -> Loc -> Status
update (am, sm) i = next
    where status = sm Map.! i
          ocs = filter (==Occupied) [ sm Map.! adj | adj <- am Map.! i ]
          next = case status of
                Empty | null ocs -> Occupied
                Occupied | length ocs >= 4 -> Empty
                _ -> status

run :: (AdjacentMap, SeatMap) -> SeatMap
run (am, sm) = Map.mapWithKey f sm
    where f key x = update (am, sm) key 

solve1 :: (AdjacentMap, SeatMap) -> Int
solve1 (am, sm) = if (nsm == sm)
    then Map.size $ Map.filter (== Occupied) nsm
    else (solve1 (am, nsm))
    where nsm = run (am, sm)

main :: IO ()
main = do
    { args <-  getArgs
    ; content <- parse $ head args
    ; putStrLn $ "Part 1: " ++ show (solve1 $ maps content)
    }
