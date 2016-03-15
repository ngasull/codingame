import System.IO
import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Foldable as F

data PhoneTree a = PhoneTree a [PhoneTree a] deriving Show
instance F.Foldable PhoneTree where
  foldMap f (PhoneTree c t) = f c `mappend` (mconcat . map (F.foldMap f) $ t)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    n <- read <$> getLine
    phones <- sort <$> replicateM n getLine

    hPrint stderr $ phones
    hPrint stderr . makeTree 'x' $ phones
    print . F.foldl (\cnt _ -> cnt+1) (-1) . makeTree 'x' $ phones

    return ()

groupByHead :: [String] -> [(Char, [String])]
groupByHead phones = [(h, map tail sub) | sub <- groups, let h = head . head $ sub]
  where groups = groupBy ((==) `on` head) phones

makeTree :: Char -> [String] -> PhoneTree Char
makeTree c phones = PhoneTree c subtrees
  where subtrees = [makeTree h (filter (not . null) sub) | (h, sub) <- groupByHead phones]
