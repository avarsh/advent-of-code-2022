import System.IO
import Data.List
import Data.Char (isUpper)

convertToPriority :: Char -> Int
convertToPriority item
  | isUpper item = (fromEnum item - fromEnum 'A') + 27
  | otherwise    = (fromEnum item - fromEnum 'a') + 1

getOverlap :: [[Int]] -> Int
getOverlap [a, b, c] = head $ intersect c (intersect a b)

main :: IO ()
main = do
  contents <- lines <$> readFile "Inputs/input3"
  print . sum $
    fmap getOverlap $
      ((snd <$>) <$>)
      <$> groupBy
        (\(a, _) (b, _) -> (a - 1) `div` 3 == (b - 1) `div` 3) 
        $ zip [1..]
        $ (convertToPriority <$>) <$> contents