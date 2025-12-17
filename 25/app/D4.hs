module D4 where
import Common

-- succumb to indexing instead of rotation
surroundingCoords :: Int -> Int -> (Int, Int) -> [(Int, Int)]
surroundingCoords n m (i, j) = [
        (i', j') |
        i' <- [(i-1)..(i+1)],
        j' <-[(j-1)..(j+1)],
        i' >= 0 && j' >= 0 && i' < n && j' < m && (i, j) /= (i', j')
    ]

index2d :: [[a]] -> (Int, Int) -> a
index2d matrix (i, j) = matrix !! i !! j

mask :: String -> [[Int]]
mask = map (map (fromEnum . (==)'@')) . lines

d4p1 :: String -> Int
d4p1 input =
    length .
    filter (<4) .
    map (sum . map (index2d bitmask) . surroundingCoords n m) .
    filter ((==1) .  index2d bitmask) .
    concat
    $ coordMatrix
    where
        bitmask = mask input
        n = length bitmask
        m = length $ head bitmask
        iota x = [0..(x-1)]
        coordMatrix = outerProduct (,) (iota n) (iota m)


d4p2 :: String -> Int
d4p2 input = length setCoords - length remaining
    where
        bitmask = mask input
        n = length bitmask
        m = length $ head bitmask
        iota x = [0..(x-1)]
        coordMatrix = outerProduct (,) (iota n) (iota m)
        setCoords = filter ((==1) .  index2d bitmask) .  concat $ coordMatrix
        remaining = until (\x -> step x == x) step setCoords
        step :: [(Int, Int)] -> [(Int, Int)]
        step cs =
            map fst .
            filter ((>=4) . snd) .
            map (\c -> (c, length . filter (`elem`cs) . surroundingCoords n m $ c))
            $ cs

d4 :: String -> String
d4 input = unlines . map (show . ($ input)) $ [d4p1, d4p2]