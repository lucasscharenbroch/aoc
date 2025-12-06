module D3 where

maxBattery2 :: String -> String
maxBattery2 s = [firstDigit, secondDigit]
    where
        firstDigit = maximum . init $ s
        secondDigit = maximum . tail . dropWhile (/=firstDigit) $ s

maxBatteryN :: Int -> String -> String
maxBatteryN 0 _ = []
maxBatteryN n s = firstDigit : maxBatteryN (n - 1) rest
    where
        firstDigit = maximum . dropTail (n - 1) $ s
        rest = tail . dropWhile (/=firstDigit) $ s
        dropTail x = reverse . drop x . reverse

d3p1 :: String -> Int
d3p1 = sum . map (read . maxBattery2) . lines

d3p2 :: String -> Int
d3p2 = sum . map (read . maxBatteryN 12) . lines

d3 :: String -> String
d3 input = unlines . map (show . ($ input)) $ [d3p1, d3p2]