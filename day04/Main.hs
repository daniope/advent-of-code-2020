module Main where

import Data.Char
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
type Valid = Bool
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

year :: Parser Int
year = fmap read $ many1 digit

value :: Parser Value
value = many1 $ choice [alphaNum, char '#']

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

isValidYear :: Field -> Bool
isValidYear (t, v) = do 
    { let y = read v
    ; case t of
        BYR -> y >= 1920 && y <= 2002
        IYR -> y >= 2010 && y <= 2020
        EYR -> y >= 2020 && y <= 2030
        _   -> False
    }

isValidHeight :: Value -> Bool
isValidHeight v = do
    { let h = read $ [d | d <- v, isDigit d]
    ; let u = drop (length v - 2) v
    ; case u of
        "cm" -> h >= 150 && h <= 193
        "in" -> h >= 59 && h <= 76
        _ -> False
    }

isValidHairColor :: Value -> Bool
isValidHairColor v = do
    { let bs = [(d `elem` ['0'..'9']) || (d `elem` ['a'..'f']) | d <- (drop 1 v)]
    ; (v !! 0 == '#') && not (False `elem` bs)
    }

isValidEyeColor :: Value -> Bool
isValidEyeColor v = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidID :: Value -> Bool
isValidID v = (length v == 9) && not (False `elem` (map isDigit v))

isValidField :: Field -> Bool
isValidField (BYR, v) = isValidYear (BYR, v)
isValidField (IYR, v) = isValidYear (IYR, v)
isValidField (EYR, v) = isValidYear (EYR, v)
isValidField (HGT, v) = isValidHeight v
isValidField (HCL, v) = isValidHairColor v 
isValidField (ECL, v) = isValidEyeColor v 
isValidField (PID, v) = isValidID v
isValidField (CID, v) = True

isValidPassport :: Passport -> Bool
isValidPassport p = hasAll p && not (False `elem` (map isValidField p))

countValid :: [Bool] -> Int
countValid bs = (sum . (map fromEnum)) bs

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile passports $ head args
    ; case result of
        Left err  -> print err
        Right ps  -> do
            { putStrLn $ "Part 1: " ++ show (countValid $ map hasAll ps)
            ; putStrLn $ "Part 2: " ++ show (countValid $ map isValidPassport ps)
            }
    }
