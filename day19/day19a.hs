import Data.List (intercalate)
import Data.Map (Map, fromList, (!))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawin <- readFile "day19/input"
  let input : strs : _ = map (`split` "\n") (split rawin "\n\n")
  let parsedInput = fromList $ map parse input

  let regex = "^" ++ buildRegex parsedInput 0 ++ "$"

  print $ length $ filter (=~ regex) strs

buildRegex :: Map Int (Either Char [[Int]]) -> Int -> String
buildRegex m i = case m ! i of
  Left chr -> [chr]
  Right xs -> "(" ++ intercalate "|" [concatMap (buildRegex m) a | a <- xs] ++ ")"

parse :: String -> (Int, Either Char [[Int]])
parse xs
  | head instr == '\"' = (read firstnum, Left (instr !! 1))
  | instr =~ "^[0-9]" = (read firstnum, Right nums)
  where
    firstnum : (_ : instr) : _ = split xs ":"
    nums = map getnums $ split instr "\\|"

-- Utility function, Return List of ints from string of space seperated ints
getnums :: String -> [Int]
getnums xs = map read nums
  where
    nums = filter (/= "") (split xs " ")

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)