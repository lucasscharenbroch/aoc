module D1 where
import Data.List
import Data.Maybe
import Data.Bifunctor

-- e.g. "L2\nR3" -> [-2, 3]
parseCombo :: String -> [Int]
parseCombo =
    map (
        uncurry (*) .
        bimap signumifyDirectionChar (read :: String -> Int) .
        fromJust .
        uncons
    ) .
    lines
    where
        signumifyDirectionChar :: Char -> Int
        signumifyDirectionChar 'L' = -1
        signumifyDirectionChar 'R' = 1

d1p1 :: String -> Int
d1p1 = length .
    filter (==0) .
    map ((`mod`100) . (+100)) .
    scanl (+) 50 .
    parseCombo

d1p2 :: String -> Int
d1p2 input = d1p1 input + crossovers input - countedTwice
    where
        -- crossovers = # times the quotient changes
        crossovers = sum .
            map (abs . uncurry (-)) .
            adjPairs .
            map (`div`100) .
            map (+(100 * 10000)) . -- pull numbers way up so we don't have to deal with negatives
            scanl (+) 50 .
            parseCombo
        adjPairs :: [a] -> [(a, a)]
        adjPairs xs = zip xs (drop 1 xs)
        -- these are included by both `crossovers` and `d1p1`
        countedTwice = length . filter id $ incToZero ++ decFromZero
        incToZero = zipWith (&&) (drop 1 zeroMask) (map (>0) combination)
        decFromZero = zipWith (&&) zeroMask (map (<0) combination)
        zeroMask = map (==0) .
            map ((`mod`100) . (+100)) .
            scanl (+) 50 $ combination
        combination = parseCombo input

d1 :: String -> String
d1 input = unlines . map show $ [d1p1 input, d1p2 input]