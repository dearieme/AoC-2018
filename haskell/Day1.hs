module Day1 where

readChange :: String -> Int
readChange ('+':xs) = read xs
readChange xs       = read xs

processChanges :: [String] -> Int
processChanges xs = sum $ map readChange xs

testChanges1 :: [String]
testChanges1 = ["+1", "+1", "+1"]

testChanges2 :: [String]
testChanges2 = ["+1", "+1", "-2"]

testChanges3 :: [String]
testChanges3 = ["-1", "-2", "-6"]
