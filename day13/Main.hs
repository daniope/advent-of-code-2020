module Main where

import Data.Maybe
import Prelude hiding (id)
import Text.Parsec
import Text.Parsec.String
import System.Environment

type Timestamp = Int
type ID = Int

integer :: Parser Int
integer = fmap read $ many1 digit

invalid :: Parser Char
invalid = char 'x'

id :: Parser (Maybe ID)
id = try (Just <$> integer) <|> (char 'x' >> return Nothing)

sep :: Parser Char
sep = char ','

ids :: Parser [Maybe ID]
ids = sepBy1 id sep

input :: Parser (Timestamp, [Maybe ID])
input = do
    { ts <- integer
    ; _ <- endOfLine
    ; ids <- ids
    ; return (ts, ids)
    }

solve1 :: Timestamp -> [ID] -> Int
solve1 ts ids = multiply $ minimum dts
    where dt x = (mod (-ts) x, x)
          dts = map dt ids
          multiply (dt, id) = dt * id

euclid :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
euclid (r, s, t) (r', s', t')
    | r' == 0 = (r, s, t)
    | otherwise = euclid (r', s', t') (mod r r', s - q * s', t - q * t')
        where q = div r r'

bezout :: (Int, Int) -> (Int, Int)
bezout (a, b) = (s, t)
    where (_, s, t) = euclid (a, 1, 0) (b, 0, 1)

chineseRemainder :: ([Int], [Int]) -> Int
chineseRemainder (ps, rs) = mod x n
    where n = foldr (*) 1 ps
          ns = map (div n) ps
          ms = map (fst . bezout) (zip ns ps)
          x = sum $ zipWith3 (\a b c -> a * b * c) rs ms ns

findTimestamp :: [(ID, Int)] -> [Int] -> Timestamp
findTimestamp (o:os) (i:is) = if valid os
    then ts
    else findTimestamp (o:os) is
    where ts = i * fst o
          valid [] = True
          valid ((id, dt):xs) = ((id - mod ts id) == dt) && valid xs

solve2 :: [Maybe ID] -> Timestamp
solve2 ids = chineseRemainder $ unzip os
    where os = [ (id, mod (-o) id) | (Just id, o) <- zip ids [0..] ]

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile input $ head args
    ; case result of
        Left err -> print err
        Right (ts, ids) -> do
            { putStrLn $ "Part 1: " ++ show (solve1 ts [ id | Just id <- ids] )
            ; putStrLn $ "Part 2: " ++ show (solve2 ids)
            }
    }
