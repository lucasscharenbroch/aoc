allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs

isSafe :: [Int] -> Bool
isSafe xs = all ((`elem`[1..3]) . abs) diffs &&
            allEqual (map signum diffs)
    where diffs = zipWith (-) (tail xs) xs

isSafe2 :: [Int] -> Bool
isSafe2 xs = any (isSafe . (`without`xs)) [0..(length xs - 1)]
    where without n ys = take n ys ++ drop (n + 1) ys

solve :: ([Int] -> Bool) -> String -> String
solve isSafe = show .  sum .  map (fromEnum . isSafe . map read . words) .  lines

main :: IO ()
main = interact (\s -> solve isSafe s ++ "\n" ++ solve isSafe2 s ++ "\n")
