import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day9/input"
  let inp = map (read :: String -> Int) (split input "\n")
  print $attack inp 25 25

attack :: [Int] -> Int -> Int -> Int
attack input pointer n
  | pointer == length input = -1
  | weakness = i
  | otherwise = attack input (pointer + 1) n
  where
    lastn = take n (drop (pointer - n) input)
    i = input !! pointer
    weakness = not $ or [True | x <- lastn, y <- lastn, x /= y, x + y == i]

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)