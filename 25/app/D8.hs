module D8 where
import Common
import Data.List
import Data.Map (Map, lookup, insert, empty, findWithDefault)
import Data.Function
import Control.Monad

parse :: String -> [(Int, Int, Int)]
parse = map (untuple . map read . split ',') . lines
    where
        untuple [a, b, c] = (a, b, c)

dist :: (Int, Int, Int) -> (Int, Int, Int) -> Double
dist (a, b, c) (d, e, f) = sqrt . fromIntegral $ (a - d)^2 + (b - e)^2 + (c - f)^2

groupOf :: Int -> Map Int Int -> Int
groupOf x m = case Data.Map.lookup x m of
    Nothing -> x
    Just x' -> if x == x' then x else groupOf x' m

unify :: Int -> Int -> Map Int Int -> Map Int Int
unify x y m = Data.Map.insert (groupOf x m) (groupOf y m) m

unify' :: Int -> Int -> Map Int Int -> (Map Int Int, Bool)
unify' x y m = (Data.Map.insert x' y' m, x' /= y')
    where
        x' = groupOf x m
        y' = groupOf y m

topNPairs = 1000
topNGroups = 3

d8p1 :: String -> Int
d8p1 input = product . take topNGroups . reverse . sort . map length . groupBy (on (==) fst) . sort $ finalGroupings
    where
        coords = parse input
        coordsAndIndices = zip coords [0 :: Int ..]
        distsAndIndices = sort . filter (uncurry (<) . snd) . concat $ outerProduct (\(c1, i1) (c2, i2) -> (dist c1 c2, (i1, i2))) coordsAndIndices coordsAndIndices
        finalGroupingMap = foldl (\m (i1, i2) -> unify i1 i2 m) empty . map snd . take topNPairs $ distsAndIndices
        indices = map snd coordsAndIndices
        finalGroupings = map (\i -> (groupOf i finalGroupingMap, i)) indices

-- have to use `unify'` to observe if `unify` had any effect. using that we can tell which was the last connection.
d8p2 :: String -> Int
d8p2 input = uncurry (on (*) (fst3 . (coords!!))) $ head mutatingUnions
    where
        coords = parse input
        coordsAndIndices = zip coords [0 :: Int ..]
        distsAndIndices = sort . filter (uncurry (<) . snd) . concat $ outerProduct (\(c1, i1) (c2, i2) -> (dist c1 c2, (i1, i2))) coordsAndIndices coordsAndIndices
        (finalGroupingMap, mutatingUnions) = foldl (\(m, xs) is@(i1, i2) -> let (m', didUnify) = unify' i1 i2 m in (m', if didUnify then is:xs else xs)) (empty, []) . map snd $ distsAndIndices
        fst3 (a, b, c) = a

d8 :: String -> String
d8 input = unlines . map (show . ($ input)) $ [d8p1, d8p2]