import System.IO
import Data.List

readInt :: String -> Int
readInt = read

convert :: String -> [[Int]] -> [[Int]]
convert xs acc@(elf : elves)
  | null xs   = [] : acc
  | otherwise = ((readInt xs):elf):elves

main :: IO ()
main = do
  snacks <- lines <$> readFile "Inputs/input1"
  print . sum . take 3 . reverse . sort $ sum <$> foldr convert [[]] snacks