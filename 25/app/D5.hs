module D5 where
import Common
import Data.Bifunctor
import Data.List

parse :: String -> ([(Int, Int)], [Int])
parse = bimap (map (untuple . map read . split '-')) (map read . tail) . break (=="") . lines
    where
        untuple [x,y] = (x,y)

inRange :: (Int, Int) -> Int -> Bool
inRange (lo, high) n = lo <= n && n <= high

d5p1 :: String -> Int
d5p1 input = length . filter (\n -> any (`inRange`n) ranges) $ nums
    where (ranges, nums)  = parse input

d5p2 :: String -> Int
d5p2 = sum . map rangeSize . mergeRanges . fst . parse
    where
        mergeRanges :: [(Int, Int)] -> [(Int, Int)]
        mergeRanges = mergeRanges' . sort
        mergeRanges' :: [(Int, Int)] -> [(Int, Int)]
        mergeRanges' [] = []
        mergeRanges' [r] = [r]
        mergeRanges' (r1@(b1, e1):r2@(b2, e2):rest)
            | e1 < b2 = r1 : mergeRanges (r2:rest)
            | otherwise = mergeRanges' $ (b1, max e1 e2) : rest
        rangeSize = (+1) . abs . uncurry (-)

d5 :: String -> String
d5 input = unlines . map (show . ($ input)) $ [d5p1, d5p2]