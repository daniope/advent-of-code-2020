module Main where

import Data.Bits hiding (bit)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String
import System.Environment

type BitMask = [(Int, Int)]
type Value = (Int, Int)

data Command = Command
    { mask :: BitMask
    , vals :: [Value]
    } deriving (Show)

type Program = [Command]
type Memory = Map Int Int

integer :: Parser Int
integer = fmap read $ many1 digit

bit :: Parser (Maybe Char)
bit = try (Just <$> digit) <|> (char 'X' >> return Nothing)

bitMask :: Parser BitMask
bitMask = do
    { _ <- string "mask = "
    ; bs <- many1 bit
    ; let l = length bs
    ; return [ (i, read [b]) | (i, Just b) <- zip [l-1,l-2..0] bs ]
    }

value :: Parser Value
value = do
    { _ <- try $ string "mem["
    ; loc <- integer
    ; _ <- string "] = "
    ; num <- integer
    ; return (loc, num)
    }

command :: Parser Command
command = do
    { m <- bitMask
    ; _ <- endOfLine
    ; vs <- endBy value endOfLine
    ; return $ Command m vs
    }

program :: Parser Program
program = many1 command

encode :: Int -> BitMask -> Int
encode x [] = x
encode x ((i, b):bm) 
    | b == 0 = encode (clearBit x i) bm
    | b == 1 = encode (setBit x i) bm

runCommand :: Memory -> Command -> Memory
runCommand mem (Command bm []) = mem
runCommand mem (Command bm ((i,x):vs)) = runCommand nmem $ Command bm vs
    where nmem = Map.insert i nx mem
          nx = encode x bm

runProgram :: Memory -> Program -> Memory
runProgram mem [] = mem
runProgram mem (c:cs) = runProgram nmem cs
    where nmem = runCommand mem c

solve :: Program -> Int
solve p = sum $ Map.elems mem
    where mem = runProgram Map.empty p

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile program $ head args
    ; case result of
        Left err -> print err
        Right r -> do
            { putStrLn $ "Part 1: " ++ show (solve r)
            }
    }
