import Data.List ( intersect )
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day6/input"
  let parsedinput = [split g "\n" | g <- split input "\n\n"]
  -- Calculate list of questions unanimously answered yes for each group
  let yesqs = [foldl1 intersect group | group <- parsedinput]
  -- Calculate result by summing counts of questions answered all yes
  print $ length $ concat yesqs

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)

frequency :: Eq a => a -> [a] -> Int
frequency n xs = length ([a | a <- xs, a == n])