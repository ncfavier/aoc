module Day04 where

import AOC

passports :: Parser [[(String, String)]]
passports = passport `sepBy` newline where
    passport = field `endBy` oneOf " \n"
    field = (,) <$> many letterChar <* ":" <*> many (noneOf " \n")

infix 1 :=>
data FieldSpec = forall a. String :=> Parser a

fieldsExist, formats :: [FieldSpec]
fieldsExist = map (:=> skipMany anySingle)
    ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
formats =
    [ "byr" :=> numberInRange (1920, 2002)
    , "iyr" :=> numberInRange (2010, 2020)
    , "eyr" :=> numberInRange (2020, 2030)
    , "hgt" :=> numberInRange (150, 193) *> "cm"
           <||> numberInRange (59, 76) *> "in"
    , "hcl" :=> "#" *> count 6 hexDigitChar
    , "ecl" :=> choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    , "pid" :=> count 9 digitChar
    ]

validate :: [FieldSpec] -> [(String, String)] -> Bool
validate formats passports = isJust do
    for_ formats \(field :=> format) -> do
        v <- lookup field passports
        () <$ parseMaybe format v

main = do
    input <- parseInput passports
    for [fieldsExist, formats] do
        print . flip howMany input . validate
