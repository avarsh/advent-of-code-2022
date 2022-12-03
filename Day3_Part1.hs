import System.IO
import Data.List
import Data.Char (isUpper)

convertToPriority :: Char -> Int
convertToPriority item
  | isUpper item = (fromEnum item - fromEnum 'A') + 27
  | otherwise    = (fromEnum item - fromEnum 'a') + 1

getOverlap :: [Int] -> Int
getOverlap xs = head $ intersect (take half xs) (drop half xs)
  where
    half = length xs `div` 2

main :: IO ()
main = do
  contents <- lines <$> readFile "Inputs/input3"
  print . sum $ getOverlap <$> (convertToPriority <$>) <$> contents