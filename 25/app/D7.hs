module D7 where
import Data.List

processLine :: String -> String -> String
processLine "" line = line
processLine processedAboveLine line = zipWith6
        (\x above aboveLeft aboveRight left right -> if shouldBePipe x above aboveLeft aboveRight left right then '|' else x)
        line
        processedAboveLine
        ('.' : processedAboveLine)
        (tail $ processedAboveLine ++ ".")
        ('.' : line)
        (tail $ line ++ ".")
    where
        shouldBePipe x above aboveLeft aboveRight left right
            | x /= '.' = False
            | otherwise = isPipe above || (isPipe aboveLeft && isSplitter left) || (isPipe aboveRight && isSplitter right)
        isPipe = (`elem` "|S")
        isSplitter = (==) '^'

-- run the splitting thing
process :: [String] -> [String]
process = tail . scanl processLine ""

-- observe number of splits from a processed grid
countSplits :: [String] -> Int
countSplits = sum . map (countSubstring "|^") . transpose
    where
        countSubstring :: String -> String -> Int
        countSubstring needle haystack = length . filter (isPrefixOf needle) $ tails haystack

d7p1 :: String -> Int
d7p1 = countSplits . process . lines

processLineDp :: [Int] -> String -> [Int]
processLineDp aboveDp line = zipWith6
        calcDp
        line
        aboveDp
        (0 : aboveDp)
        (tail $ aboveDp ++ [0])
        ('.' : line)
        (tail $ line ++ ".")
    where
        calcDp x above aboveLeft aboveRight left right
            | x == 'S' = 1
            | x /= '.' = 0
            | otherwise = above + (aboveLeft * fromEnum (isSplitter left)) + (aboveRight * fromEnum (isSplitter right))
        isSplitter = (==) '^'

processDp :: [String] -> [Int]
processDp = foldl processLineDp (repeat 0)

d7p2 :: String -> Int
d7p2 = sum . processDp . lines

d7 :: String -> String
d7 input = unlines . map (show . ($ input)) $ [d7p1, d7p2]