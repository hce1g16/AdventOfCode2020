import Data.Function (on)
import Data.List (intersect, nub, (\\))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawinput <- readFile "day17/input"
  -- Options: Number of dimensions, number of cycles to run
  let (dimensions, cycles) = (4, 6)
  -- Parse grid
  let grid = split rawinput "\n"
  let initGrid = [[x, y] | x <- [0 .. length (head grid) - 1], y <- [0 .. length grid - 1], grid !! y !! x == '#']
  let active = map (++ replicate (dimensions - 2) 0) initGrid
  -- Calculate & print result
  print $ length (iterate nextactive active !! cycles)

-- Given a list of n-dimensional coordinates, calculate the next iteration
nextactive :: [[Int]] -> [[Int]]
nextactive xs = [a | a <- nub (neighbours ++ xs), willBeActive a xs]
  where
    neighbours = concatMap getNeighbours xs

-- Calculate whether a point will be active in the next iteration
willBeActive :: [Int] -> [[Int]] -> Bool
willBeActive coords active
  | isActive && activeNeighbours `elem` [2, 3] = True
  | not isActive && activeNeighbours == 3 = True
  | otherwise = False
  where
    isActive = coords `elem` active
    activeNeighbours = length $ getNeighbours coords `intersect` active

-- Given a coordinate of any dimensions, return a list of the neighbouring points
getNeighbours :: [Int] -> [[Int]]
getNeighbours coords = map (zipWith (+) coords) diffs \\ [coords]
  where
    diffs = sequence (replicate (length coords) [-1 .. 1])

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)