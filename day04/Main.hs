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

fieldSep :: Parser Char
fieldSep = char ':'

passportSep :: Parser Char
passportSep = choice [space, endOfLine]

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
    }

value :: Parser Value
value = manyTill anyChar $ try passportSep

field :: Parser Field
field = do
    { t <- tag
    ; _ <- fieldSep
    ; v <- value
    ; return $ (t, v)
    }

passport :: Parser Passport
passport = many1 field

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile passport $ head args
    ; print result
--    ; case result of
--        Left err  -> print err
--        Right xs  -> do
--            { putStr "Part 1: "
--            ; print $ count_valid $ map is_1_valid xs
--            ; putStr "Part 2: "
--            ; print $ count_valid $ map is_2_valid xs
--            }
    }
