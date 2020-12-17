import Data.List (intersect, nub)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawinput <- readFile "day17/input"
  -- Parse the grid
  let grid = split rawinput "\n"
  let active = [(x, y, 0) | x <- [0 .. length (head grid) - 1], y <- [0 .. length grid - 1], grid !! y !! x == '#']
  -- Calculate and print the result
  print $ length (iterate nextactive active !! 6)

-- Calculate the active corrdinates in the next iteration
nextactive :: [(Int, Int, Int)] -> [(Int, Int, Int)]
nextactive xs = [a | a <- nub (neighbours ++ xs), willBeActive a xs]
  where
    neighbours = concatMap getNeighbours xs

-- Calculate whether a point will be active in the next iteration
willBeActive :: (Int, Int, Int) -> [(Int, Int, Int)] -> Bool
willBeActive coords active
  | isActive && activeNeighbours `elem` [2, 3] = True
  | isActive = False
  | not isActive && activeNeighbours == 3 = True
  | not isActive = False
  where
    isActive = coords `elem` active
    activeNeighbours = length $ getNeighbours coords `intersect` active

-- Return a list of neighboring points
getNeighbours :: (Int, Int, Int) -> [(Int, Int, Int)]
getNeighbours (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1 .. 1], dy <- [-1 .. 1], dz <- [-1 .. 1], (dx, dy, dz) /= (0, 0, 0)]

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)