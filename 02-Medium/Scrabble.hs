import System.IO
import Control.Applicative
import Control.Monad
import Data.Function
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    n <- read <$> getLine
    wds <- replicateM n getLine
    letters <- getLine

    let possibleWords = filter (contains letters) wds
        scores = [(foldl (\cnt c -> cnt + score c) 0 w, w) | w <- possibleWords]
        -- maximumBy takes the last best, so reverse
        best = snd . maximumBy (compare `on` fst) . reverse $ scores

    putStrLn best
    return ()

contains :: (Eq a, Ord a, Show a) => [a] -> [a] -> Bool
contains xa xb = contains' (sort xa) (sort xb)

contains' :: (Eq a, Show a) => [a] -> [a] -> Bool
contains' _ [] = True
contains' [] _ = False
contains' (a:ra) xb@(b:rb)
  | a == b    = contains' ra rb
  | otherwise = contains' ra xb

score :: Char -> Int
score c = case find (elem c . fst) scoreMap of
  Just (_, s) -> s
  Nothing -> error $ "No score found for letter " ++ [c]

scoreMap :: [(String, Int)]
scoreMap = [ ("eaionrtlsu", 1)
           , ("dg", 2)
           , ("bcmp", 3)
           , ("fhvwy", 4)
           , ("k", 5)
           , ("jx", 8)
           , ("qz", 10)
           ]
