import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day12/input"
  let inp = split input "\n"
  print $ follow inp

follow :: [[Char]] -> Int
follow directions = abs x + abs y
  where
    (x, y) = follow' directions (0, 0) 90

follow' :: [[Char]] -> (Int, Int) -> Int -> (Int, Int)
follow' [] (x, y) _ = (x, y)
follow' ((dir : diststr) : xs) (x, y) bearing
  | dir == 'R' = follow' xs (x, y) (bearing + dist)
  | dir == 'L' = follow' xs (x, y) (bearing - dist)
  | dir == 'N' = follow' xs (x, y + dist) bearing
  | dir == 'S' = follow' xs (x, y - dist) bearing
  | dir == 'E' = follow' xs (x + dist, y) bearing
  | dir == 'W' = follow' xs (x - dist, y) bearing
  | dir == 'F' = follow' xs (x + (dist * xvect), y + (dist * yvect)) bearing
  where
    dist = (read :: String -> Int) diststr
    (xvect, yvect) = dire bearing

-- Convert angle to vector of integer
dire :: Int -> (Int, Int)
dire degrees = (round $ sin radians, round $ cos radians)
  where
    radians = fromIntegral degrees * (pi / 180)

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)