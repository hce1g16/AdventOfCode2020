import Data.List
import Data.Map (fromList, member, (!))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day10/input"
  let inp = sort $ map (read :: String -> Int) (split input "\n")
  print $ fst $ numRoutesTo (maximum inp) inp []

-- Calculate the number of possible routes to the target number
numRoutesTo :: Int -> [Int] -> [(Int, Int)] -> (Int, [(Int, Int)])
numRoutesTo 0 _ tups = (1, (0, 1) : tups)
numRoutesTo to inp tups
  | member to tupMap = (tupMap ! to, tups)
  | to `elem` inp = (numRoutes, newTuples)
  | otherwise = (0, tups)
  where
    tupMap = fromList tups
    (n3, t3) = numRoutesTo (to - 3) inp tups
    (n2, t2) = numRoutesTo (to - 2) inp t3
    (n1, t1) = numRoutesTo (to - 1) inp t2
    numRoutes = n3 + n2 + n1
    newTuples = (to, numRoutes) : t1

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)