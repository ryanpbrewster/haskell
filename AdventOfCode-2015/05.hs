-- 05.hs
import Data.List (group, isInfixOf)
main = do
  lines <- fmap lines (readFile "05.input")
  print $ length $ filter isNice lines

vowels = "aeiou"
isVowel ch = ch `elem` vowels

badStrings = ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice s =
  let hasAtLeastThreeVowels = length (filter isVowel s) >= 3
      hasRepeatedLetters = any (\g -> length g > 1) $ group s
      containsABadString = any (`isInfixOf` s) badStrings
  in hasAtLeastThreeVowels && hasRepeatedLetters && (not containsABadString)
