module Main where

import Data.Char
import Text.Parsec
import Text.Parsec.String
import System.Environment

data Tag =
      BYR -- Birth Year
    | IYR -- Issue Year
    | EYR -- Expiration Year
    | HGT -- Height
    | HCL -- Hair Color
    | ECL -- Eye Color
    | PID -- Passport ID
    | CID -- Country ID
    deriving (Enum, Eq, Show)

type Form = String
type Group = [Form]
type Groups = [Group]

tag :: Parser Tag
tag = do
    { s <- many1 letter
    ; return $ case s of
        "byr" -> BYR
        "iyr" -> IYR
        "eyr" -> EYR
        "hgt" -> HGT
        "hcl" -> HCL
        "ecl" -> ECL
        "pid" -> PID
        "cid" -> CID
        _     -> error "Invalid tag"
    }

form :: Parser Form
form = many1 letter

group :: Parser Group
group = endBy1 form endOfLine

groups :: Parser Groups
groups = sepBy group newline

main :: IO ()
main = do
    { args <- getArgs
    ; groups <- parseFromFile groups $ head args
    ; case groups of
        Left err  -> print err
        Right gs  -> do
            { putStrLn $ "Part 1: " ++ show gs
            }
    }
