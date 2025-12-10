module Main where
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))
import System.IO
import Text.Read (readMaybe)
import D1
import D2
import D3
import D4
import D5
import D6
-- import D7
-- import D8
-- import D9
-- import D10
-- import D11
-- import D12
-- import D13
-- import D14
-- import D15
-- import D16
-- import D17
-- import D18
-- import D19
-- import D20
-- import D21
-- import D22
-- import D23
-- import D24
-- import D25

argsToDay :: [String] -> Maybe Int
argsToDay [x] = readMaybe x
argsToDay _ = Nothing

panicWithMessage :: String -> IO ()
panicWithMessage message = do
    hPutStrLn stderr message
    exitWith (ExitFailure 1)

solutions :: [(Int, String -> String)]
solutions =
    [
        (1, d1),
        (2, d2),
        (3, d3),
        (4, d4),
        (5, d5),
        (6, d6)
--        (7, d7)
--        (8, d8)
--        (9, d9)
--        (10, d10)
--        (11, d11)
--        (12, d12)
--        (13, d13)
--        (14, d14)
--        (15, d15)
--        (16, d16)
--        (17, d17)
--        (18, d18)
--        (19, d19)
--        (20, d20)
--        (21, d21)
--        (22, d22)
--        (23, d23)
--        (24, d24)
--        (25, d25)
    ]

main :: IO ()
main = do
    args <- getArgs
    case argsToDay args of
        Nothing -> panicWithMessage "Supply the day (1-25) as an argument"
        Just day -> case lookup day solutions of
            Nothing -> panicWithMessage $ "No such day " ++ show day
            Just solution -> interact solution
