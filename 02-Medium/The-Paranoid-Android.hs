import System.IO
import Control.Applicative
import Control.Monad
import Data.Function (on)
import Data.List (find, groupBy, sortBy)
import Data.Maybe
import Debug.Trace

type Location = (Int, Int) -- floor, pos
type ElevatorData = [[Location]]
type Node = (Int, Int, Int, Int, Int) -- id, floor, pos, dir, rounds, clones

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

  turnInfo@(clonefloor, clonepos, clonedir) <- getTurnInfo

  let start = (clonefloor, clonepos, clonedir, nbrounds, nbtotalclones)
      exit = (exitfloor, exitpos)
      elevators = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ exit:allElevators

  let pathToExit = removeNeutralNodes clonedir . translateDirections . fromJust . makeGraph exit elevators $ start
  hPrint stderr pathToExit
  loop pathToExit turnInfo

loop :: [Node] -> (Int, Int, Int) -> IO ()
loop pathToExit turnInfo = do
  -- hPutStrLn stderr "Debug messages..."
  let (action, newPath) = getAction pathToExit turnInfo

  -- action: WAIT or BLOCK
  putStrLn action

  nextTurnInfo <- getTurnInfo
  loop newPath nextTurnInfo

getTurnInfo :: IO (Int, Int, Int)
getTurnInfo = do
  input_line <- getLine
  let input = words input_line
      clonefloor = read (input!!0) :: Int
      clonepos = read (input!!1) :: Int
      clonedir = case input!!2 of "RIGHT" -> 1
                                  _ -> -1
  return (clonefloor, clonepos, clonedir)

makeGraph :: Location -> ElevatorData -> Node -> Maybe [Node]
-- makeGraph exit elevators node@(f, p, d, r, c) | trace (show . filter checkResources . findSiblings' exit elevators $ (f, p, -d, r-3, c-1)) False = undefined
makeGraph exit elevators node@(f, p, d, r, c) =
  if (f, p) == exit
  then Just [node] -- Exit
  else
    let findSiblings = findSiblings' exit elevators
        candidates = filter checkResources . concat $ [findSiblings node, findSiblings (f, p, -d, r-3, c-1)]
        siblings = map (makeGraph exit elevators) candidates
    in case find isJust siblings of
      Just (Just []) -> Nothing
      Just (Just viableFollowup) -> Just $ node:viableFollowup
      _ -> Nothing

findSiblings' :: Location -> ElevatorData -> Node -> [Node]
findSiblings' exit elevators (f, p, d, r, c) =
  let sortFn = (if d > 0 then id else flip) compare `on` snd
      floorElevators = fromMaybe [] . find ((== f) . fst . (!!0)) $ elevators
      nextElevators = sortBy sortFn . filter (\(_, ep) -> (ep-p)*d >= 0 ) $ floorElevators
  in case nextElevators of
    [] -> []
    e@(ef, ep):_ -> if e == exit
      then [(ef, ep, d, r - abs (p-ep), c)]
      else [(ef+1, ep, d, r - abs (p-ep) - 1, c)]

checkResources :: Node -> Bool
checkResources (_, _, _, r, c) = all (>= 0) [r, c]

translateDirections :: [Node] -> [Node]
translateDirections [] = []
translateDirections [node] = [node]
translateDirections ((f, p, _, r, c):node2@(_, _, d2, _, _):rest) =
  (f, p, d2, r, c):translateDirections (node2:rest)

removeNeutralNodes :: Int -> [Node] -> [Node]
removeNeutralNodes _ [] = []
removeNeutralNodes d (node@(_, _, nd, _, _):rest)
  | nd == d = removeNeutralNodes d rest
  | otherwise = node:removeNeutralNodes (-d) rest

getAction :: [Node] -> (Int, Int, Int) -> (String, [Node])
getAction [] _ = ("WAIT", [])
getAction nodes@((nf, np, _, _, _):rest) (f, p, _) =
  if nf == f && np == p
    then ("BLOCK", rest)
    else ("WAIT", nodes)

-- getAction nodes (f, p, d) =
--   case find (\(nf, np, _, _, _) -> nf == f && np == p) nodes of
--     Just (_, _, nd, _, _) -> if nd == d then "WAIT" else "BLOCK"
--     Nothing -> "WAIT"
--   where wait = ("WAIT", )
