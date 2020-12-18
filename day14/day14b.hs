import Data.Bits (Bits (clearBit, setBit))
import Data.Map (Map, elems, empty, fromList, union)
import GHC.Int (Int64)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawinput <- readFile "day14/input"
  let input = split rawinput "\n"
  print $ sum $ elems (run input "" empty)

-- Run the program, args are instruction list, mask and memory
run :: [String] -> String -> Map Int Int -> Map Int Int
run [] _ mem = mem
run (ins : xs) mask mem
  | instruction == "mask" = run xs value mem
  | otherwise = run xs mask newmem
  where
    (instruction, _, value) = ins =~ " = " :: (String, String, String)
    (_, memaddressstr, _) = instruction =~ "[0-9]+" :: (String, String, String)
    memaddress = read memaddressstr :: Int
    memaddresslist = applymask mask memaddress
    newmem = fromList (zip memaddresslist (replicate (length memaddresslist) (read value :: Int))) `union` mem

-- Takes a mask and memory address, returns all memory addresses to write to
applymask :: String -> Int -> [Int]
applymask mask num = applymask' (zip (reverse mask) [0 ..]) [fromIntegral num :: Int64]

applymask' :: [(Char, Int)] -> [Int64] -> [Int]
applymask' [] vals = map fromIntegral vals
applymask' ((chr, indx) : xs) vals
  | chr == 'X' = applymask' xs (map (`setBit` indx) vals ++ map (`clearBit` indx) vals)
  | chr == '1' = applymask' xs (map (`setBit` indx) vals)
  | chr == '0' = applymask' xs vals

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)