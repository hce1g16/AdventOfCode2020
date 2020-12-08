import Data.Map (Map, fromList, (!))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day7/input"
  let parsedinput = [parseLine g | g <- split input "\n"]
  let bagmappings = fromList parsedinput
  print $ length [col | (col, _) <- parsedinput, checkContains col "shiny gold" bagmappings]

-- Check whether a bag can contain another bag, based on a map of valid bag content
checkContains :: String -> String -> Map String [(String, Int)] -> Bool
checkContains colour colourin bmap
  | colourin `elem` subbags = True
  | otherwise = or [checkContains c colourin bmap | c <- subbags]
  where
    subbags = map fst (bmap ! colour)

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