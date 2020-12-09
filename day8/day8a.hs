import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day8/input"
  let program = map parseInstruction (split input "\n")
  print $ run program [] 0 0

-- Run the program and return the acc value at the point where the program loops
run :: [(String, Int)] -> [Int] -> Int -> Int -> Int
run prog seen p acc
  | p `elem` seen = acc
  | op == "nop" = run prog (p : seen) (p + 1) acc
  | op == "acc" = run prog (p : seen) (p + 1) (acc + int)
  | op == "jmp" = run prog (p : seen) (p + int) acc
  where
    (op, int) = prog !! p

-- Parses a instruction row into Operator and Integer
parseInstruction :: String -> (String, Int)
parseInstruction prog = (op, int)
  where
    (op, _, intstring) = prog =~ " \\+?" :: (String, String, String)
    int = (read :: String -> Int) intstring

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)
