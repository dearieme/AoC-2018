module Day1 where

import qualified Data.Map   as Map
import           Data.Maybe

readChange :: String -> Int
readChange ('+':xs) = read xs
readChange xs       = read xs

sumChanges :: [String] -> Int
sumChanges xs = sum $ map readChange xs

findFirstRepeat :: Map.Map Int Bool -> [Int] -> Int
findFirstRepeat seen (x:xs) =
  if isNothing $ Map.lookup x seen
    then findFirstRepeat (Map.insert x True seen) xs
    else x

getRepeat :: [String] -> Int
getRepeat xs = findFirstRepeat Map.empty runningTotals
  where
    cycledInputs = cycle $ map readChange xs
    runningTotals = scanl (+) 0 cycledInputs

main :: IO ()
main = do
  input <- readFile "day1_inputs.txt"
  let inputList = lines input
  putStr "Resulting frequency: "
  print $ sumChanges inputList
  putStr "First repeated frequency: "
  print $ getRepeat inputList
