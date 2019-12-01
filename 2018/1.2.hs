import qualified Data.Set as S
import System.Exit
import Control.Monad
import Data.List

main = do
    fs <- scanl' (+) 0 . cycle . map (read . dropWhile (== '+')) . lines <$> getContents :: IO [Integer]
    let f s n = do
            when (S.member n s) $ die (show n)
            return $ S.insert n s
    foldM f S.empty fs
