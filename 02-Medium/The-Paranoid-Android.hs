import System.IO
import Control.Monad
import Data.Function (on)
import Data.List (groupBy, sortBy)

type Location = (Int, Int) -- floor, pos
type ElevatorData = [[Location]]
type Node = (Int, Int, Int, Int, Int) -- floor, pos, dir, rounds, clones
data Graph = Graph Node [Graph] | Exit

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Auto-generated code below aims at helping you parse
  -- the standard input according to the problem statement.

  input_line <- getLine
  let input = words input_line
  let nbfloors = read (input!!0) :: Int -- number of floors
  let width = read (input!!1) :: Int -- width of the area
  let nbrounds = read (input!!2) :: Int -- maximum number of rounds
  let exitfloor = read (input!!3) :: Int -- floor on which the exit is found
  let exitpos = read (input!!4) :: Int -- position of the exit on its floor
  let nbtotalclones = read (input!!5) :: Int -- number of generated clones
  let nbadditionalelevators = read (input!!6) :: Int -- ignore (always zero)
  let nbelevators = read (input!!7) :: Int -- number of elevators

  allElevators <- forM [1..nbelevators] $ \_ -> do
    input_line <- getLine
    let input = words input_line
    let elevatorfloor = read (input!!0) :: Int -- floor on which this elevator is found
    let elevatorpos = read (input!!1) :: Int -- position of the elevator on its floor
    return (elevatorfloor, elevatorpos)

  firstTurnInfo@(clonefloor, clonepos, clonedir) <- getTurnInfo

  let start = (clonefloor, clonepos, 1, nbrounds, nbtotalclones)
      resources = (nbrounds, nbtotalclones)
      elevators = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ allElevators
      graph = makeGraph (exitfloor, exitpos) elevators start

  loop (clonefloor, clonepos, 1)

loop :: (Int, Int, Int) -> IO ()
loop (clonefloor, clonepos, _) = do
  let start = ()
      graph = ()

  -- hPutStrLn stderr "Debug messages..."

  -- action: WAIT or BLOCK
  putStrLn "WAIT"

  nextTurnInfo <- getTurnInfo
  loop nextTurnInfo

getTurnInfo :: IO (Int, Int, Int)
getTurnInfo = do
  input_line <- getLine
  let input = words input_line
      clonefloor = read (input!!0) :: Int
      clonepos = read (input!!1) :: Int
      clonedir = case input!!2 of "RIGHT" -> 1
                                  _ -> -1
  return (clonefloor, clonepos, clonedir)

makeGraph :: Location -> ElevatorData -> Node -> Graph
makeGraph exit@(exitfloor, exitpos) elevators node@(f, p, d, r, c) =
  if f == exitfloor && p == exitpos
  then Exit
  else
    let findSiblings = findSiblings' exit elevators
        candidates = filter checkResources . concat $ [findSiblings node, findSiblings (f, p, -d, r-3, c-1)]
    in Graph node (map (makeGraph exit elevators) candidates)

findSiblings' :: Location -> ElevatorData -> Node -> [Node]
findSiblings' exit elevators (f, p, d, r, c) =
  let sortFn = (if d > 0 then id else flip) compare `on` fst
      nextElevators = sortBy sortFn . filter ((>= 0) . (* d) . snd) $ exit:(elevators!!f)
  in case nextElevators of
    [] -> []
    _ -> [(ef+1, ep, d, r - abs (p-ep) - 1, c)]
      where (ef, ep) = head nextElevators

checkResources :: Node -> Bool
checkResources (_, _, d, r, c) = all (>= 0) [d, r, c]
