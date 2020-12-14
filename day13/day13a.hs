import Data.Function
import Data.List (minimumBy)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawinput <- readFile "day13/input"
  -- parse input
  let (rawtarget, _, rawschedule) = rawinput =~ "\n" :: (String, String, String)
  let target = (read :: String -> Int) rawtarget
  let schedule = [(read :: String -> Int) c | c <- split rawschedule ",", c /= "x"]

  -- Calc answer
  let (bus, arrival) = minimumBy (compare `on` snd) [(x, x * ((target `div` x) + 1)) | x <- schedule]
  print $ (arrival - target) * bus

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)