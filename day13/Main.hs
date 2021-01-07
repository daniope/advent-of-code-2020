module Main where

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

input :: Parser (Timestamp, [ID])
input = do
    { ts <- integer
    ; _ <- endOfLine
    ; is <- ids
    ; return (ts, [ i | Just i <- is ])
    }

solve :: Timestamp -> [ID] -> Int
solve ts ids = multiply $ minimum dts
    where dt x = (x - mod ts x, x)
          dts = map dt ids
          multiply (dt, id) = dt * id

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile input $ head args
    ; case result of
        Left err -> print err
        Right (ts, ids) -> do
            { putStrLn $ "Part 1: " ++ show (solve ts ids)
            }
    }
