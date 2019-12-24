module Day08 where

import AOC

(width, height) = (25, 6)
size = width * height

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

main = do
    layers <- chunksOf size . map digitToInt . dropWhileEnd isSpace <$> getContents
    let (_, n1, n2) = minimumBy (\(a, _, _) (b, _, _) -> a `compare` b) (map count layers)
    print (n1 * n2)
    let image = foldr1 render layers
    mapM_ putStrLn [map (" █?" !!) row | row <- chunksOf width image]
