module Day08 where

import AOC

import Data.Map qualified as M

main :: IO ()
main = do
  (fmap digitToInt -> grid, width, height) <- makeGrid' <$> readInput
  let inBounds = inRange ((0, 0), (width-1, height-1))
  let visibleFrom f = go (-1) 0 where
        go v j | not (inBounds p) = []
               | let t = grid M.! p, t > v = (p, True):go t (j+1)
               | otherwise = (p, False):go v (j+1)
               where p = f j
      visible = M.fromListWith (||) $ concat $
        [visibleFrom (,i) ++ visibleFrom (\j -> (width-j-1, i)) | i <- [0..height-1]] ++
        [visibleFrom (i,) ++ visibleFrom (\j -> (i, height-j-1)) | i <- [0..width-1]]
  print $ howMany id visible
  let viewingDistance f = go 1 where
        t = grid M.! f 0
        go n | not (inBounds p) = n-1
             | grid M.! f n >= t = n
             | otherwise = go (n+1)
             where p = f n
      scenicScore (i, j) = viewingDistance (\n -> (i+n, j))
                         * viewingDistance (\n -> (i-n, j))
                         * viewingDistance (\n -> (i, j+n))
                         * viewingDistance (\n -> (i, j-n))
  print $ maximum $ fmap scenicScore (M.keys grid)
