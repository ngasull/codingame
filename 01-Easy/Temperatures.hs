import System.IO
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Debug.Trace

zeroestTemp :: [Int] -> Int
-- zeroestTemp temps | trace (show temps) False = undefined
zeroestTemp [] = 0
zeroestTemp temps =
    let min_abs = minimum . map abs $ temps
    in
    maximum . filter (\t -> abs t == min_abs) $ temps

parseTemps :: [Char] -> [Int]
parseTemps "" = []
parseTemps input = map read . splitOn " " $ input

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    getLine
    input_temps <- getLine
    -- the n temperatures expressed as integers ranging from -273 to 5526

    -- hPutStrLn stderr "Debug messages..."

    -- Write answer to stdout
    -- print . show $ (map read . splitOn " " $ temps :: [Int])
    putStrLn . show . zeroestTemp . parseTemps $ input_temps
    return ()
