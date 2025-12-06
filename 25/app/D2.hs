module D2 where
import Common
import Data.List

solve :: (String -> Bool) -> String -> Int
solve invalid = sum . filter (invalid . show) . concatMap ((\l -> [(l !! 0)..(l !! 1)]) . map read . split '-') . split ','

d2p1 :: String -> Int
d2p1 = solve isReplicate2

isReplicate2 :: String -> Bool
isReplicate2 s = even sz && all (uncurry (==)) (zip s $ drop (sz`div`2) s)
    where
        sz = length s

d2p2 :: String -> Int
d2p2 = solve isReplicate

isReplicate :: String -> Bool
isReplicate s = any (and . zipWith (==) s . cycle) . filter ((\l -> l > 0 && l < sz && sz`mod`l == 0) . length) $ inits s
    where sz = length s

d2 :: String -> String
d2 input = unlines . map show $ [d2p1 input, d2p2 input]