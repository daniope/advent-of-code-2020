module Main where

import Text.Parsec
import Text.Parsec.Char
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

type Value = String
type Field = (Tag, Value)
type Passport = [Field]

tag :: Parser Tag
tag = do
    { s <- count 3 letter
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

value :: Parser Value
value = many1 $ choice [alphaNum, (char '#')]

field :: Parser Field
field = do
    { t <- tag
    ; _ <- char ':'
    ; v <- value
    ; _ <- choice [space, endOfLine]
    ; return $ (t, v)
    }

passport :: Parser Passport
passport = many1 field

passports :: Parser [Passport]
passports = sepBy passport newline

hasAll :: Passport -> Bool
hasAll p = length (filter ((/=CID).fst) p) == 7

countValid :: [Bool] -> Int
countValid bs = (sum . (map fromEnum)) bs

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile passports $ head args
    ; case result of
        Left err  -> print err
        Right ps  -> putStrLn $ "Part 1: " ++ show (countValid $ map hasAll ps)
    }
