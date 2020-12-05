import Data.List

main :: IO ()
main = do
  input <- readFile "day5/input"
  let l = lines input
  let seatAllocations = [findSeat i (0, 7) (0, 127) | i <- l]
  print $ findMissingSeat $ sort seatAllocations

findMissingSeat :: [Int] -> [Int]
findMissingSeat [_] = []
findMissingSeat (a : b : xs)
  | b - a == 2 = b -1 : findMissingSeat (b : xs)
  | otherwise = findMissingSeat (b : xs)

findSeat :: String -> (Int, Int) -> (Int, Int) -> Int
findSeat [] (xmin, _) (ymin, _) = ymin * 8 + xmin
findSeat (x : xs) (xmin, xmax) (ymin, ymax)
  | x == 'F' = findSeat xs (xmin, xmax) (ymin, ymax - ydiff)
  | x == 'B' = findSeat xs (xmin, xmax) (ymin + ydiff, ymax)
  | x == 'L' = findSeat xs (xmin, xmax - xdiff) (ymin, ymax)
  | x == 'R' = findSeat xs (xmin + xdiff, xmax) (ymin, ymax)
  where
    xdiff = ((- xmin + xmax) `div` 2) + 1
    ydiff = ((- ymin + ymax) `div` 2) + 1