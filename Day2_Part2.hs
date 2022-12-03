import System.IO
import Data.List
import qualified Data.Map as M
import Data.Maybe

score :: Char -> Char -> Int
score x y = fromJust $ M.lookup (x, y) $ M.fromList
  [(('A', 'Z'), 6 + 2)   -- Rock, Win [Paper]
  ,(('A', 'Y'), 3 + 1)   -- Rock, Draw [Rock]
  ,(('A', 'X'), 0 + 3)   -- Rock, Lose [Scissors] 
  ,(('C', 'Z'), 6 + 1)   -- Scissors, Win [Rock]
  ,(('C', 'Y'), 3 + 3)   -- Scissors, Draw [Scissors]
  ,(('C', 'X'), 0 + 2)   -- Scissors, Lose [Paper]
  ,(('B', 'Z'), 6 + 3)   -- Paper, Win [Scissors]
  ,(('B', 'Y'), 3 + 2)   -- Paper, Draw [Paper]
  ,(('B', 'X'), 0 + 1)   -- Paper, Lose [Rock]
  ] 

getRoundOutcome :: String -> Int
getRoundOutcome round = score x y
  where
    ([x]:[y]:[]) = words round
      

main :: IO ()
main = do
  strategy <- lines <$> readFile "Inputs/input2"
  print . sum $ getRoundOutcome <$> strategy