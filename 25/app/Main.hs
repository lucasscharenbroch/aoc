module Main where
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))
import System.IO
import Text.Read (readMaybe)
import D1

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
        (1, d1)
    ]

main :: IO ()
main = do
    args <- getArgs
    case argsToDay args of
        Nothing -> panicWithMessage "Supply the day (1-25) as an argument"
        Just day -> case lookup day solutions of
            Nothing -> panicWithMessage $ "No such day " ++ show day
            Just solution -> interact solution
