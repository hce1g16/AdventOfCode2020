import Text.Regex.TDFA ((=~))

data Operator
  = Number Int
  | Add
  | Multiply

main :: IO ()
main = do
  rawinput <- readFile "day18/input"
  let input = split [a | a <- rawinput, a /= ' '] "\n"
  print $ sum $ map compute input

-- Convert input string to list of operators, bracket contents are evaluated into a number
parse :: String -> [Operator]
parse [] = []
parse (x : xs)
  | x == '+' = Add : parse xs
  | x == '*' = Multiply : parse xs
  | [x] =~ "[0-9]" = Number (read [x]) : parse xs
  | x == '(' = Number (compute group) : parse (drop 1 xs')
  where
    (group, xs') = untilCloseBracket xs

-- Splits the given string at the point where the current bracket layer ends
untilCloseBracket :: String -> (String, String)
untilCloseBracket xs = splitAt (i -1) xs
  where
    i = head [n | n <- [1 .. length xs], let st = take n xs, frequency ')' st > frequency '(' st]

-- Takes an input string, parses and returns result
compute :: String -> Int
compute xs = n
  where
    Number n = head $ eval $ parse xs

-- Evaluate the operator list, will return a list containing the resultant number
eval :: [Operator] -> [Operator]
eval [n] = [n]
eval (n : operator : n2 : xs) = case (n, operator, n2) of
  (Number n, Multiply, Number n2) -> eval (Number (n * n2) : xs)
  (Number n, Add, Number n2) -> eval (Number (n + n2) : xs)

-- Utility function, calculate frequency of element in the given list
frequency :: Eq a => a -> [a] -> Int
frequency n (x : xs) = length ([a | a <- x : xs, a == n])

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)