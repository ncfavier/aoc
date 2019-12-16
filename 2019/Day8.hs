module Day8 where

import Data.List
import Data.Char

(width, height) = (25, 6)
size = width * height

charToInt c = fromEnum c - fromEnum '0'

splitLayers [] = []
splitLayers layers = layer:splitLayers rest
    where (layer, rest) = splitAt size layers

count = foldl' f (0, 0, 0)
    where
        f (n0, n1, n2) 0 = (n0 + 1, n1, n2)
        f (n0, n1, n2) 1 = (n0, n1 + 1, n2)
        f (n0, n1, n2) 2 = (n0, n1, n2 + 1)
        f c _ = c

render = zipWith f
    where
        f 2 c = c
        f c _ = c

draw [] = return ()
draw image = do
    putStrLn $ map (" █?" !!) row
    draw rest
    where (row, rest) = splitAt width image

main = do
    layers <- splitLayers . map charToInt . dropWhileEnd isSpace <$> getContents
    let (_, n1, n2) = minimumBy (\(a, _, _) (b, _, _) -> a `compare` b) (map count layers)
    print (n1 * n2)
    let image = foldr1 render layers
    draw image
