main :: IO ()
main = do
  g <- readFile "day1/input"
  let l = lines g
  let m = map (read :: String -> Int) l
  let answer = checkExpense' m 2020
  print answer

checkExpense' :: [Int] -> Int -> Int
checkExpense' (x : xs) target
  | result /= -1 = result * x
  | otherwise = checkExpense' xs target
  where
    remainder = target - x
    result = checkExpense xs remainder
checkExpense' [] _ = -1

checkExpense :: [Int] -> Int -> Int
checkExpense (x : xs) target
  | result /= -1 = result
  | otherwise = checkExpense xs target
  where
    result = checkSum x xs target
checkExpense [] _ = -1

checkSum :: Int -> [Int] -> Int -> Int
checkSum x (y : ys) target
  | x + y == target = x * y
  | otherwise = checkSum x ys target
checkSum _ [] _ = -1