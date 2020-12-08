import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  input <- readFile "day8/input"
  -- Generate permutations
  let permutations = permutateProg (split input "\n") 0
  -- Print the acc value after program terminates for the first permutation that doesn't loop
  print $ head [acc | i <- permutations, let (loops, acc) = doesLoop i [] 0 0, not loops]

-- Given a program, return a list of every possible single jmp <-> nop permutation of the program
permutateProg :: [String] -> Int -> [[String]]
permutateProg prog p
  | p == length prog = []
  | op == "nop" = replaceVal p ("jmp " ++ intstr) prog : permutateProg prog (p + 1)
  | op == "jmp" = replaceVal p ("nop " ++ intstr) prog : permutateProg prog (p + 1)
  | op == "acc" = permutateProg prog (p + 1)
  where
    (op, int) = parseInstruction (prog !! p)
    intstr = if int > 0 then "+" ++ show int else show int

-- Returns (True, acc value at loop) if the program loops, (false, acc value at end) otherwise
doesLoop :: [String] -> [Int] -> Int -> Int -> (Bool, Int)
doesLoop prog seen p acc
  | p == length prog = (False, acc)
  | p `elem` seen = (True, acc)
  | op == "nop" = doesLoop prog (p : seen) (p + 1) acc
  | op == "acc" = doesLoop prog (p : seen) (p + 1) (acc + int)
  | op == "jmp" = doesLoop prog (p : seen) (p + int) acc
  where
    (op, int) = parseInstruction (prog !! p)

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

-- Utility function, returns the last n elements of the given list
lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse

replaceVal :: Int -> a -> [a] -> [a]
replaceVal i v xs = take i xs ++ [v] ++ lastN (length xs - i - 1) xs