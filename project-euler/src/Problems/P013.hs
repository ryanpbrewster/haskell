module Problems.P013
  ( process
  ) where

-- 013.hs
type FileContents = String

process :: FileContents -> String
process txt =
  let ans = sum $ map read $ lines txt
  in take 10 $ show ans
