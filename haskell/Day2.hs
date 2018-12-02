module Day2 where

import qualified Data.Map as Map

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

main :: IO ()
main = do
  input <- readFile "day2_inputs.txt"
  let inputList = lines input
  putStr "Checksum: "
  print $ checksum inputList
