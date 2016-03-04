{-# LANGUAGE NamedFieldPuns #-}

import           Control.Monad
import           System.IO

data Turn = Turn { x0 :: Int
                 , x1 :: Int
                 , y0 :: Int
                 , y1 :: Int
                 , x  :: Int
                 , y  :: Int
                 , n  :: Int
                 }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    input_wh <- getLine
    input_n <- getLine
    input_xy <- getLine

    let [w, h] = map read . words $ input_wh :: [Int]
        n = read input_n :: Int -- maximum number of turns before game over.
        [x, y] = map read . words $ input_xy :: [Int]

    loop Turn { x=x, y=y, x0=0, y0=0, x1=w-1, y1=h-1, n=n }

loop :: Turn -> IO ()
loop turn = do
    bomb_dir <- getLine -- the direction of the bombs from batman's current location (U, UR, R, DR, D, DL, L or UL)
    let nextTurn@Turn {x,y} = optimalNextTurn turn $ readDirection bomb_dir
    -- hPutStrLn stderr "Debug messages..."

    -- the location of the next window Batman should jump to.
    putStrLn . concat $ [show x, " ", show y]
    loop nextTurn

optimalNextTurn :: Turn -> (Int, Int) -> Turn
optimalNextTurn turn@Turn {x0, x1, y0, y1, x, y} (dx, dy) =
  turn {x0=nx0, x1=nx1, y0=ny0, y1=ny1, x=nx, y=ny}
  where nx0 = if dx > 0 then min (x+1) x1 else x0
        nx1 = if dx < 0 then max (x-1) x0 else x1
        ny0 = if dy > 0 then min (y+1) y1 else y0
        ny1 = if dy < 0 then max (y-1) y0 else y1
        nx = (nx0 + nx1) `div` 2
        ny = (ny0 + ny1) `div` 2

readDirection :: [Char] -> (Int, Int)
readDirection (c:[])
  | c `elem` "UD" = readDirection [c, 'X']
  | otherwise = readDirection ['X', c]
readDirection (cy:cx:[]) = (toIntDirection cx, toIntDirection cy)

toIntDirection :: Char -> Int
toIntDirection c
  | c `elem` "DR" = 1
  | c `elem` "UL" = -1
  | otherwise = 0
