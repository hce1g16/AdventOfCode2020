import Data.Map (Map, fromList, member)
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

-- Fields which are required for passports
mandatory :: [String]
mandatory = ["byr", "iyr", "eyr", "iyr", "hgt", "hcl", "ecl", "pid"]

-- Check whether the given passport contains the mandatory fields
checkPassport :: Map String String -> Bool
checkPassport passport = and ([member field passport | field <- mandatory])

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)