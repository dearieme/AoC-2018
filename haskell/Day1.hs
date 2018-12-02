module Day1 where

readChange :: String -> Int
readChange ('+':xs) = read xs
readChange xs       = read xs

processChanges :: [String] -> Int
processChanges xs = sum $ map readChange xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ processChanges (lines input)
