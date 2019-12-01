import qualified Data.Set as S
import Data.Foldable
import Control.Monad
import System.Exit

main = do
    ws <- lines <$> getContents
    let rm i l = take i l ++ tail (drop i l)
    let f s w = do
            when (S.member w s) $ die w
            return $ S.insert w s
    sequence_ [foldM f S.empty (rm i <$> ws) | i <- [0..25]]
