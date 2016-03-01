import System.IO
import Control.Monad
import Data.List

data Cell = Cell Int Int | Empty deriving (Eq, Show)
data Matrix = Matrix Int Int [[Cell]] deriving (Show)

cellMapping :: Int -> Int -> Char -> Cell
cellMapping x y '0' = Cell x y
cellMapping _ _ '.' = Empty

handleCell :: Matrix -> Cell -> IO ()
handleCell _ Empty = do return ()
handleCell (Matrix _ _ m) c = do
    putStrLn $ concat [getCellString c, " ", getCellString nx, " ", getCellString ny]
    where (Cell x y) = c
          nx = nextNeighbour . drop (x+1) $ (transpose m)!!y
          ny = nextNeighbour . drop (y+1) $ m!!x

getCellString :: Cell -> [Char]
getCellString (Cell x y) = concat [show x, " ", show y]
getCellString Empty = "-1 -1"

nextNeighbour :: [Cell] -> Cell
nextNeighbour cells
    | null cells = Empty
    | next_cell == Empty = nextNeighbour rest
    | otherwise = next_cell
    where next_cell:rest = cells

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Don"t let the machines win. You are humanity's last hope...

    input_line <- getLine
    hPutStrLn stderr input_line
    let width = read input_line :: Int -- the number of cells on the X axis
    input_line <- getLine
    hPutStrLn stderr input_line
    let height = read input_line :: Int -- the number of cells on the Y axis

    cell_lines <- flip mapM [0..height-1] $ \y -> do
        line <- getLine
        hPutStrLn stderr line
        -- width characters, each either 0 or .
        return [ cellMapping x y (line!!x) | x <- [0..length line-1] ]

    let matrix = Matrix width height (transpose cell_lines)
    flip mapM (concat cell_lines) $ handleCell matrix


    -- hPutStrLn stderr "Debug messages..."

    -- Three coordinates: a node, its right neighbor, its bottom neighbor
    -- putStrLn "0 0 1 0 0 1"
    return ()
