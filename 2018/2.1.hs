import Control.Arrow
import Data.Monoid
import Data.Function
import Data.List

main = do
    let count l a = length (filter (== a) l)
    print . uncurry ((*) `on` getSum) . foldMap (((Sum . fromEnum . any (== 2)) &&& (Sum . fromEnum . any (== 3))) . (<$> ['a'..'z']) . count) . lines =<< getContents
