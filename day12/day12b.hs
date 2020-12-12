import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day12/input"
  let inp = split input "\n"
  print $ follow inp

follow :: [[Char]] -> Int
follow directions = abs x + abs y
  where
    (x, y) = follow' directions (0, 0) (10, 1)

follow' :: [[Char]] -> (Int, Int) -> (Int, Int) -> (Int, Int)
follow' [] (x, y) _ = (x, y)
follow' ((dir : diststr) : xs) (x, y) (wx, wy)
  | dir == 'R' = follow' xs (x, y) (rot (- dist) (wx, wy))
  | dir == 'L' = follow' xs (x, y) (rot dist (wx, wy))
  | dir == 'N' = follow' xs (x, y) (wx, wy + dist)
  | dir == 'S' = follow' xs (x, y) (wx, wy - dist)
  | dir == 'E' = follow' xs (x, y) (wx + dist, wy)
  | dir == 'W' = follow' xs (x, y) (wx - dist, wy)
  | dir == 'F' = follow' xs (x + xdir, y + ydir) (wx, wy)
  where
    dist = (read :: String -> Int) diststr
    xdir = wx * dist
    ydir = wy * dist

-- Rotates the given point around (0, 0) by x degrees
rot :: Int -> (Int, Int) -> (Int, Int)
rot degrees (wx, wy) = (round newx, round newy)
  where
    radians = fromIntegral degrees * (pi / 180)
    (wxInt, wyInt) = (fromIntegral wx, fromIntegral wy)
    newx = cos radians * wxInt - sin radians * wyInt
    newy = sin radians * wxInt + cos radians * wyInt

-- Rotates a point about a given point
rot' :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
rot' degrees (x, y) (wx, wy) = (round newx, round newy)
  where
    radians = fromIntegral degrees * (pi / 180)
    (xInt, yInt) = (fromIntegral x, fromIntegral y)
    (wxInt, wyInt) = (fromIntegral wx, fromIntegral wy)
    newx = cos radians * (wxInt - xInt) - sin radians * (wyInt - yInt) + xInt
    newy = sin radians * (wxInt - xInt) + cos radians * (wyInt - yInt) + yInt

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)