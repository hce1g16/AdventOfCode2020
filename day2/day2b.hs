import System.IO
import Data.List
import Text.Regex.TDFA

main = do
     g <- readFile "day2/input"
     let l = lines(g)
     -- parse the input and pass each value of it to checkPassword 
     let parsed = [(i =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: (String, String, String, [String])) | i <- l] 
     let matches = [checkPassword pw char (read low :: Int) (read high :: Int) | (_, _, _, low:high:(char:_):pw:_) <- parsed]
     let result = sum matches     
     print result

-- Checks that a password has char in one of pos p1 or p1 but not both
checkPassword :: String -> Char -> Int -> Int -> Int

checkPassword pw char p1 p2
    | xor (ele1 == char) (ele2 == char) = 1
    | otherwise                         = 0
    where ele1 = pw !! (p1-1)
          ele2 = pw !! (p2-1)

xor :: Bool -> Bool -> Bool

xor a b
    | (a == b) = False
    | otherwise = True
