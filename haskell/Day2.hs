module Day2 where

import           Data.List  (intersect)
import qualified Data.Map   as Map
import           Data.Maybe

charFrequency :: String -> [Int]
charFrequency xs = Map.elems $ go Map.empty xs
  where
    go :: Map.Map Char Int -> String -> Map.Map Char Int
    go freqMap [] = freqMap
    go freqMap (x:xs) = go newMap xs
      where
        newMap = Map.insertWith (+) x 1 freqMap

numWithFreq :: Int -> [String] -> Int
numWithFreq n xs = length $ filter (== True) $ map (withFreq n) xs
  where
    withFreq :: Int -> String -> Bool
    withFreq n xs = elem n $ charFrequency xs

checksum :: [String] -> Int
checksum xs = hasTwo * hasThree
  where
    hasTwo = numWithFreq 2 xs
    hasThree = numWithFreq 3 xs

diffId :: String -> String -> Int
diffId id1 id2 = length differences
  where
    differences = filter (uncurry (/=)) $ zip id1 id2

isDiffByOne :: String -> String -> Bool
isDiffByOne id1 id2 = diffId id1 id2 == 1

diffByOne :: Monad m => m String -> m (Maybe (String, String))
diffByOne xs = do
  id1 <- xs
  id2 <- xs
  if isDiffByOne id1 id2
    then return $ Just (id1, id2)
    else return Nothing

diffByOnes :: [String] -> (String, String)
diffByOnes xs = fromJust $ head $ filter isJust $ diffByOne xs

main :: IO ()
main = do
  input <- readFile "day2_inputs.txt"
  let inputList = lines input
  putStr "Checksum: "
  print $ checksum inputList

  let pairDiffByOne = diffByOnes inputList
  putStr "IDs differing by one char: "
  print $ diffByOnes inputList
  putStr "Common chars are: "
  print $ uncurry intersect pairDiffByOne

