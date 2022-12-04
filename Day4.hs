import System.IO
import Control.Arrow
import Data.Tuple
import Control.Monad
import Data.List

splitAtChar :: Char -> String -> (String, String)
splitAtChar x = (id *** tail) . ((uncurry (&&&) . (takeWhile &&& dropWhile)) (/= x))

readInt :: String -> Int
readInt = read

both = join (***)

doesContainLower :: (String, String) -> Bool
doesContainLower = (uncurry (<=)) . 
  both ((readInt . fst . (splitAtChar '-'))) 

doesContainUpper:: (String, String) -> Bool
doesContainUpper = (uncurry (>=)) . 
  both ((readInt . snd . (splitAtChar '-')))

doesContain :: (String, String) -> Bool
doesContain = uncurry (||) .
  ((uncurry (&&) . (doesContainLower &&& doesContainUpper)) &&&
   (uncurry (&&) . (doesContainLower &&& doesContainUpper) . swap))

doesOverlap :: (String, String) -> Bool
doesOverlap pair = (not . null) (intersect [fst as .. snd as] [fst bs .. snd bs])
  where 
    as = both readInt $ (splitAtChar '-') (fst pair)
    bs = both readInt $ (splitAtChar '-') (snd pair)

part1 :: [String] -> Int
part1 pairs = sum ((fromEnum . doesContain . (splitAtChar ',')) <$> pairs)

part2 :: [String] -> Int
part2 pairs = sum ((fromEnum . doesOverlap . (splitAtChar ',')) <$> pairs)

main :: IO ()
main = do
  pairs <- lines <$> readFile "Inputs/input4"
  print $ part1 pairs
  print $ part2 pairs