import Intcode

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

run :: [Integer] -> Integer -> IO Integer
run program input = last <$> runIntcode program [input]

main :: IO ()
main = do
    program <- map read . split ',' <$> readFile "input5"
    print =<< run program 1
    print =<< run program 5
