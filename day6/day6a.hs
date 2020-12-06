import Data.List (union)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day6/input"
  let parsedinput = [split g "\n" | g <- split input "\n\n"]
  -- Calculate list of questions answered yes for each group
  let yesqs = [foldl1 union group | group <- parsedinput]
  -- Calculate result by summing counts of questions answered yes
  print $ length $ concat yesqs

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)