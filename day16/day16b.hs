import Data.Function (on)
import Data.List (sortBy, (\\))
import Data.Ord (comparing)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawinput <- readFile "day16/input"

  -- Parsing the input
  let [rawrestrictions, rawticket, rawothertickets] = split rawinput "\n\n(:?[a-z ]+:\n)?"

  let restrictionsMap = map parseRestrictionsMap (split rawrestrictions "\n")
  let restrictions = map snd restrictionsMap

  let myticket = map read (split rawticket ",") :: [Int]

  let othertickets = [map read i :: [Int] | i <- map (`split` ",") (split rawothertickets "\n")]

  -- Filter out invalid tickets
  let allvalidtickets = [t | t <- othertickets, and [or [r f | r <- restrictions] | f <- t]]

  -- Figure out which fields are possible for each column, based on the values in each column
  let options = [(i, [field | (field, r) <- restrictionsMap, all r col]) | i <- [0 .. length (head allvalidtickets) -1], let col = map (!! i) allvalidtickets]
  let sortOptions = sortBy (compare `on` (length . snd)) options

  -- Calculate the column allocations
  let results = findCombo sortOptions []

  -- Calculate and print the result
  print $ product [myticket !! i | (i, colstr) <- results, colstr =~ "departure" :: Bool]

findCombo :: [(Int, [String])] -> [String] -> [(Int, String)]
findCombo [] _ = []
findCombo ((i, options) : xs) used
  | null available = [(-1, "-1")]
  | null upcoming && xs /= [] = [(-1, "-1")]
  | otherwise = (i, option) : upcoming
  where
    available = options \\ used
    (upcoming, option) = head ([(u, o) | o <- available, let u = findCombo xs (o : used), let lastu = if not (null u) then fst $ last u else 1, lastu /= -1] ++ [([], "a")])

-- Helper function for parsing a restriction line to a (field , validation function) tuple
parseRestrictionsMap :: String -> (String, Int -> Bool)
parseRestrictionsMap line = (field, \x -> ((min1 <= x) && (x <= max1)) || ((min2 <= x) && (x <= max2)))
  where
    (_, _, _, field : rawparams) = line =~ "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: (String, String, String, [String])
    min1 : max1 : min2 : max2 : _ = map read rawparams :: [Int]

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)