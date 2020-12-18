import Data.Bits (Bits ((.&.), (.|.)))
import Data.Map (Map, elems, empty, insert)
import GHC.Int (Int64)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  rawinput <- readFile "day14/input"
  let input = split rawinput "\n"
  print $ sum $ elems (run input (0, 0) empty)

-- Run the program, args are instruction list, mask and memory
run :: [String] -> (Int64, Int64) -> Map Int Int -> Map Int Int
run [] _ mem = mem
run (ins : xs) mask mem
  | instruction == "mask" = run xs (parsemask value) mem
  | otherwise = run xs mask (insert memaddress toWrite mem)
  where
    (instruction, _, value) = ins =~ " = " :: (String, String, String)
    (_, memaddressstr, _) = instruction =~ "[0-9]+" :: (String, String, String)
    memaddress = read memaddressstr :: Int
    toWrite = applymask (read value :: Int64) mask

-- Apply the given mask to the given int
applymask :: Int64 -> (Int64, Int64) -> Int
applymask num (ormask, andmask) = fromIntegral $ ((num .|. ormask) .&. andmask) `mod` (2 ^ 36)

-- Parses a mask string into a mask tuple
parsemask :: String -> (Int64, Int64)
parsemask xs = (fromIntegral (oneraw `mod` (2 ^ 36)) :: Int64, fromIntegral (zeroraw `mod` (2 ^ 36)) :: Int64)
  where
    (oneraw, zeroraw) = parsemask' (zip (reverse xs) [0 ..]) 0 0

parsemask' :: [(Char, Int)] -> Int -> Int -> (Int, Int)
parsemask' [] val1s val0s = (val1s, val0s)
parsemask' ((chr, indx) : xs) val1s val0s
  | chr == 'X' = parsemask' xs val1s (val0s + (^) 2 indx)
  | chr == '1' = parsemask' xs (val1s + (^) 2 indx) (val0s + (^) 2 indx)
  | chr == '0' = parsemask' xs val1s val0s

-- Utility function, splits string based on regex
split :: String -> String -> [String]
split [] _ = []
split str regex = before : split upcoming regex
  where
    (before, _, upcoming) = str =~ regex :: (String, String, String)