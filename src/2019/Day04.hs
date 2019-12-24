module Day04 where

import AOC

main = do
    [l] <- lines <$> getContents
    let range = (low, up) where (low, '-':up) = break (== '-') l
    mapM_ print $ do
        (?) <- [(>=), (==)]
        return $ length $ do
            d1 <- ['1'..'6']
            d2 <- [d1..'9']
            d3 <- [d2..'9']
            d4 <- [d3..'9']
            d5 <- [d4..'9']
            d6 <- [d5..'9']
            let n = [d1, d2, d3, d4, d5, d6]
            guard (fst range <= n && n <= snd range)
            guard (any (\g -> length g ? 2) (group n))
            return n
