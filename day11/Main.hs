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

type SeatMap = Map ID Status
type AdjacentMap = Map ID [ID]
type MapSize = (Int, Int)

toStatus :: Char -> Status
toStatus '.' = Floor
toStatus 'L' = Empty
toStatus '#' = Occupied
toStatus _   = error "Invalid status"

readMap :: FilePath -> IO [String]
readMap = fmap lines . readFile

getSize :: [String] -> MapSize
getSize ss = (length ss, length $ head ss)

setStatus :: [String] -> Loc -> Status
setStatus ss loc = toStatus $ (ss !! fst loc) !! snd loc

getID :: MapSize -> Loc -> ID
getID (_, cols) (i, j) = (i * cols) + j

getAdjacent :: MapSize -> Loc -> [ID]
getAdjacent size (row, col) = ids
    where id = getID size (row, col)
          ids  = 
            [ x
            | r <- [row-1..row+1]
            , c <- [col-1..col+1]
            , r >= 0 && r < fst size
            , c >= 0 && c < snd size
            , let x = getID size (r, c)
            , x /= id
            ]

maps :: [String] -> (AdjacentMap, SeatMap)
maps ss = (am, sm)
    where size = getSize ss
          ls = [ (i , j) | i <- [0..(fst size)-1], j <- [0..(snd size)-1]]
          makeAdjacent x = (getID size x, getAdjacent size x)
          makeSeat x = (getID size x, setStatus ss x)
          am = Map.fromList (map makeAdjacent ls)
          sm = Map.fromList (map makeSeat ls)

update :: (AdjacentMap, SeatMap) -> ID -> Status
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
    ; content <- readMap $ head args
    ; putStrLn $ "Part 1: " ++ show (solve1 $ maps content)
    }
