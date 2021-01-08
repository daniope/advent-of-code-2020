module Main where

import Data.Bits hiding (bit)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String
import System.Environment

type BitMask = [(Int, Char)]
type Value = (Int, Int)
type Bits = String

data Command = Command
    { mask :: BitMask
    , vals :: [Value]
    } deriving (Show)

type Program = [Command]
type Memory = Map Int Int

integer :: Parser Int
integer = fmap read $ many1 digit

bit :: Parser Char
bit = oneOf ['0','1','X'] -- try (Just <$> digit) <|> (char 'X' >> return Nothing)

bitMask :: Parser BitMask
bitMask = do
    { _ <- string "mask = "
    ; bs <- many1 bit
    ; let l = length bs
    ; return $ zip [l-1,l-2..0] bs
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

decode1 :: Int -> BitMask -> Int
decode1 x [] = x
decode1 x ((i, b):bm) = decode1 nx bm
    where nx = case b of
            '0' -> clearBit x i 
            '1' -> setBit x i
            'X' -> x

runCommand :: Memory -> Command -> Memory
runCommand mem (Command bm []) = mem
runCommand mem (Command bm ((i,x):vs)) = runCommand nmem $ Command bm vs
    where nmem = Map.insert i nx mem
          nx = decode1 x bm

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
