-- 10.hs
import Data.List (group)

main = do
  let xs = map length $ iterate lookAndSay "31221"
  print $ xs !! (50 - 1)
  
lookAndSay :: String -> String
lookAndSay xs =
  concat [ show (length g) ++ [head g] | g <- group xs ]
