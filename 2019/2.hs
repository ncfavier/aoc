import Intcode

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

main :: IO ()
main = do
    program <- map read . split ',' <$> readFile "input2"
    print $ runNounVerb program 12 2
    print $ head [ 100 * noun + verb
                 | noun <- [0..99]
                 , verb <- [0..99]
                 , runNounVerb program noun verb == 19690720
                 ]
