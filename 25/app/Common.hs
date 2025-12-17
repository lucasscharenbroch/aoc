module Common where

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

split :: Char -> String -> [String]
split _ [] = []
split c s = takeWhile (/=c) s : split c (drop 1 $ dropWhile (/=c) s)

outerProduct :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outerProduct f as bs = [[f a b | b <- bs] | a <- as]