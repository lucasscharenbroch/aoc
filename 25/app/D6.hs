module D6 where
import Common
import Data.Bifunctor
import Data.List
import Data.Function

type Operation = [Int] -> Int

parse :: String -> ([[Int]], [Operation])
parse = bimap (transpose . map (map read)) (map readSign . concat) . break ((`elem`["*", "+"]) . head) . map words . lines

stupidParse :: String -> ([[Int]], [Operation])
stupidParse input = (nums, signs)
    where signs = map readSign . concat . dropWhile (not . (`elem`["*", "+"]) . head) . map words . lines $ input
          nums = map (map read) . filter (not . null) . map concat . groupBy (on (&&) (not . null)) . map words . transpose . init . lines $ input

readSign "+" = sum
readSign "*" = product

solve :: (String -> ([[Int]], [Operation])) -> String -> Int
solve p input = sum . zipWith ($) ops $ nums
    where
        (nums, ops) = p input

d6p1 :: String -> Int
d6p1 = solve parse
d6p2 :: String -> Int
d6p2 = solve stupidParse

d6 :: String -> String
d6 input = unlines . map (show . ($ input)) $ [d6p1, d6p2]