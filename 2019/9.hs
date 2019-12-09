import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> readFile "input9"
    print . head =<< runIntcode program [1]
    print . head =<< runIntcode program [2]
