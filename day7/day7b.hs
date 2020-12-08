import Data.Map (Map, fromList, (!))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day7/input"
  let bagmappings = fromList [parseLine g | g <- split input "\n"]
  print $ countBags "shiny gold" bagmappings - 1

-- Count the number of bags contained in a given bag (inckluding the bag itself)
countBags :: String -> Map String [(String, Int)] -> Int
countBags colour bmap
  | null subbags = 1
  | otherwise = 1 + sum [num * countBags col bmap | (col, num) <- subbags]
  where
    subbags = bmap ! colour

-- Parse a line of the input into tuple colour -> list of (colour, quantity)
parseLine :: String -> (String, [(String, Int)])
parseLine line
  | bagsraw == "no other bags." = (bag, [])
  | otherwise = (bag, parseBags bags)
  where
    (bag, _, bagsraw) = line =~ " bags contain " :: (String, String, String)
    bags = split bagsraw " bags?(, |\\.)"

-- Convert the contains part of the string into list of colour -> quantity
parseBags :: [String] -> [(String, Int)]
parseBags [] = []
parseBags (x : xs) = (bag, amount) : parseBags xs
  where
    (amountstr, _, bag) = x =~ " " :: (String, String, String)
    amount = (read :: String -> Int) amountstr

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)

frequency :: Eq a => a -> [a] -> Int
frequency n xs = length ([a | a <- xs, a == n])