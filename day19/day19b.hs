import Data.List (intercalate, nub)
import Data.Map (Map, fromList, (!))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawin <- readFile "day19/input2"
  let input : strs : _ = map (`split` "\n") (split rawin "\n\n")
  let parsedInput = fromList $ map parse input

  let regex = buildRegex parsedInput 0 15 0 ++ "$"

  print $ length $ filter (=~ regex) strs

buildRegex :: Map Int (Either Char [[Int]]) -> Int -> Int -> Int -> String
buildRegex instrmap depth limit index
  | depth == limit = ""
  | otherwise = case value of
    Left chr -> [chr]
    Right xs -> "(" ++ intercalate "|" (nub [b | a <- xs, let b = concatMap (buildRegex instrmap (depth + 1) limit) a, b /= ""]) ++ ")"
  where
    value = instrmap ! index

parse :: String -> (Int, Either Char [[Int]])
parse xs
  | head instr == '\"' = (read firstnum, Left (instr !! 1))
  | instr =~ "^[0-9]" = (read firstnum, Right nums)
  where
    firstnum : (_ : instr) : _ = split xs ":"
    nums = map getnums $ split instr "\\|"

getnums :: String -> [Int]
getnums xs = map read nums
  where
    nums = filter (/= "") (split xs " ")

-- Utility function, calculate frequency of element in the given list
frequency :: Eq a => a -> [a] -> Int
frequency n (x : xs) = length ([a | a <- x : xs, a == n])

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)