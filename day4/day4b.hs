import Data.Map (Map, fromList, member, (!))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day4/input"
  -- Parse the passports to get a 2d list of "field:value"
  let unparsedpassports = [split upw "[\n ]" | upw <- split input "\n\n"]
  -- Create a list of field:value passport maps
  let passports = [fromList [(field, value) | rawfield <- rawfields, let [field, value] = split rawfield ":"] | rawfields <- unparsedpassports]
  -- Calculate how many of the passports are valid
  print $ sum [1 | passport <- passports, checkPassport passport]

-- List of (field, validation function) tuples
mandatory :: [(String, String -> Bool)]
mandatory =
  [ ("byr", strIsBetween 1920 2002),
    ("iyr", strIsBetween 2010 2020),
    ("eyr", strIsBetween 2020 2030),
    ("hgt", checkHeight),
    ("hcl", \x -> x =~ "^#[a-f0-9]{6}$" :: Bool),
    ("ecl", (`elem` valideyecolours)),
    ("pid", \x -> x =~ "^[0-9]{9}$" :: Bool)
  ]

-- List of valid eye colours
valideyecolours :: [String]
valideyecolours = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

-- Checks that each mandatory field is present and the value passes the validation function
checkPassport :: Map String String -> Bool
checkPassport passport = and ([member field passport && validationfunction (passport ! field) | (field, validationfunction) <- mandatory])

-- Checks if the height is inbetween the valid limits
checkHeight :: String -> Bool
checkHeight heightstr
  | format == "cm" = strIsBetween 150 193 value
  | format == "in" = strIsBetween 59 76 value
  | otherwise = False
  where
    len = length heightstr
    value = take (len - 2) heightstr
    format = lastN 2 heightstr

-- Wrapper for isBetween that takes a string, converts to Int before calling isBetween
strIsBetween :: Int -> Int -> String -> Bool
strIsBetween low high numstr = isBetween low high num
  where
    num = (read :: String -> Int) numstr

-- Utility function, returns True if low <= num <= high
isBetween :: Int -> Int -> Int -> Bool
isBetween low high num = num >= low && num <= high

-- Utility function, returns the last n elements of the given list
lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)
