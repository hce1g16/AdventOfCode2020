main :: IO ()
main = do
  g <- readFile "day3/input"
  let l = lines g
  print $ checkSlope 0 3 1 l

checkSlope :: Int -> Int -> Int -> [String] -> Int
checkSlope pos r d (row : upcoming)
  | length upcoming < d = collision
  | otherwise = collision + checkSlope nextpos r d (drop (d - 1) upcoming)
  where
    tile = row !! pos
    collision = if tile == '#' then 1 else 0
    nextpos = (pos + r) `mod` length row