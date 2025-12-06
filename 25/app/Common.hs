module Common where

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

split :: Char -> String -> [String]
split _ [] = []
split c s = takeWhile (/=c) s : split c (drop 1 $ dropWhile (/=c) s)