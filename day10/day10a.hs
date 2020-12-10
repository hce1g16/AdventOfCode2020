import Data.List (sort)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day10/input"
  let inp = sort $ map (read :: String -> Int) (split input "\n")
  let diffs = calcdiff inp 0
  print $ (sum [1 | x <- diffs, x == 1]) * sum [1 | x <- diffs, x == 3]

calcdiff :: [Int] -> Int -> [Int]
calcdiff [] _ = [3]
calcdiff (x : xs) i = (x - i) : calcdiff xs x

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)