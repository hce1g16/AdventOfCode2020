import Data.Map (Map, fromList, insert, member, (!))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawinput <- readFile "day16/input"
  -- Parse the input
  let [rawrestrictions, _, rawothertickets] = split rawinput "\n\n(:?[a-z ]+:\n)?"

  let restrictions = map parseRestrictions (split rawrestrictions "\n")
  let allticketvals = map read (concatMap (`split` ",") (split rawothertickets "\n")) :: [Int]

  -- Calculate and print the result
  print $ sum [i | i <- allticketvals, not $ or [r i | r <- restrictions]]

-- Converts a restriction line to a Int -> Bool validation function
parseRestrictions :: String -> Int -> Bool
parseRestrictions line = \x -> ((min1 <= x) && (x <= max1)) || ((min2 <= x) && (x <= max2))
  where
    (_, _, _, rawparams) = line =~ "[a-z ]+: ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: (String, String, String, [String])
    min1 : max1 : min2 : max2 : _ = map read rawparams :: [Int]

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)