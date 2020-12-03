import Text.Regex.TDFA

main :: IO ()
main = do
  g <- readFile "day2/input"
  let l = lines g
  -- parse the input and pass args from it to checkPassword
  let parsed = [(i =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: (String, String, String, [String])) | i <- l]
  let matches = [checkPassword pw char (read low :: Int) (read high :: Int) | (_, _, _, low : high : (char : _) : pw : _) <- parsed]
  let result = sum matches
  print result

-- Checks that a password has between low and high occurances of char
checkPassword :: String -> Char -> Int -> Int -> Int
checkPassword pw char low high
  | freq <= high && freq >= low = 1
  | otherwise = 0
  where
    freq = frequency (char, pw)

frequency (n, x : xs) = length ([a | a <- x : xs, a == n])