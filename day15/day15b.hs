import Data.Map (Map, fromList, insert, member, (!))

main :: IO ()
main = do
  let input = [16, 1, 0, 18, 12, 14, 19]
  let initMap = fromList $ zip input (zip [1 ..] (repeat 0))
  print $ run initMap (last input) 30000000 (length input + 1)

run :: Map Int (Int, Int) -> Int -> Int -> Int -> Int
run mp last stop i
  | i == stop = num
  | otherwise = num `seq` run nxtmp num stop $! (i + 1)
  where
    (lastIndex, beforeLastIndex) = if member last mp then mp ! last else (0, 0)
    num = if beforeLastIndex > 0 then lastIndex - beforeLastIndex else 0
    (numlastIndex, _) = if member num mp then mp ! num else (0, 0)
    nxtmp = insert num (i, numlastIndex) mp