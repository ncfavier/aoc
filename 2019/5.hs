import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> readFile "input5"
    print . last =<< runIntcode program [1]
    print . last =<< runIntcode program [5]
