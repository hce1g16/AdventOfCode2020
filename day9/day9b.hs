import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day9/input"
  let inp = map (read :: String -> Int) (split input "\n")
  let target = attack inp 25 25
  print $ head [v | i <- [2 .. length inp], let v = attack2 inp 0 i target, v > -1]

attack :: [Int] -> Int -> Int -> Int
attack input pointer n
  | pointer == length input = -1
  | weakness = i
  | otherwise = attack input (pointer + 1) n
  where
    lastn = take n (drop (pointer - n) input)
    i = input !! pointer
    weakness = not $ or [True | x <- lastn, y <- lastn, x /= y, x + y == i]

attack2 :: [Int] -> Int -> Int -> Int -> Int
attack2 input pointer n target
  | pointer == length input = -1
  | sum lastn == target = minimum lastn + maximum lastn
  | otherwise = attack2 input (pointer + 1) n target
  where
    lastn = take n (drop (pointer - n) input)

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)