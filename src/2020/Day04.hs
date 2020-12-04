module Day04 where

import AOC

passports :: Parser [[(String, String)]]
passports = passport `sepBy` newline where
    passport = field `endBy` oneOf " \n"
    field = (,) <$> many letterChar <* ":" <*> many (noneOf " \n")

formats =
    [ "byr" ==> numberInRange (1920, 2002)
    , "iyr" ==> numberInRange (2010, 2020)
    , "eyr" ==> numberInRange (2020, 2030)
    , "hgt" ==> numberInRange (150, 193) *> "cm" <||> numberInRange (59, 76) *> "in"
    , "hcl" ==> "#" *> count 6 hexDigitChar
    , "ecl" ==> choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    , "pid" ==> count 9 digitChar
    ] where
        infix 0 ==>
        a ==> b = (a, () <$ b)

fieldsExist = map (skipMany anySingle <$) formats

validate :: [(String, Parser ())] -> [(String, String)] -> Bool
validate formats passports = isJust do
    for formats \(field, format) -> do
        v <- lookup field passports
        parseMaybe format v

main = do
    input <- parseInput passports
    print $ howMany (validate fieldsExist) input
    print $ howMany (validate formats) input
