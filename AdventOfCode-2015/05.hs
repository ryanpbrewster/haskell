-- 05.hs
import Data.List (group, isInfixOf, tails)
main = do
  lines <- fmap lines (readFile "05.input")
  print $ length $ filter isNice1 lines
  print $ length $ filter isNice2 lines

vowels = "aeiou"
isVowel ch = ch `elem` vowels

badStrings = ["ab", "cd", "pq", "xy"]

isNice1 :: String -> Bool
isNice1 s =
  let hasAtLeastThreeVowels = length (filter isVowel s) >= 3
      hasRepeatedLetters = any (\g -> length g > 1) $ group s
      containsABadString = any (`isInfixOf` s) badStrings
  in hasAtLeastThreeVowels && hasRepeatedLetters && (not containsABadString)


-- Find all substrings of length k that exist at least twice without overlaps
nonOverlappingSubstringPairs :: String -> Int -> [String]
nonOverlappingSubstringPairs s k =
  [ substr | s' <- tails s
           , length s' >= 2*k
           , let (substr, rest) = splitAt k s'
           , substr `isInfixOf` rest ]

-- Find all palindromic substrings of s that have length k
palindromicSubstrings :: String -> Int -> [String]
palindromicSubstrings s k =
  filter isPalindromic $ substrings k s

isPalindromic :: String -> Bool
isPalindromic s = s == reverse s

substrings :: Int -> String -> [String]
substrings k s
  | length s < k = []
  | otherwise = take k s : substrings k (tail s)

isNice2 :: String -> Bool
isNice2 s =
  let hasPairOfPairs = not $ null $ nonOverlappingSubstringPairs s 2
      hasXYX = not $ null $ palindromicSubstrings s 3
  in hasPairOfPairs && hasXYX
