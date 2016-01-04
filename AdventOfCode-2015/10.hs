-- 10.hs
import Data.List (group)

main = do
  let xs = map length $ iterate lookAndSay "1113222113"
  print $ xs !! 40
  print $ xs !! 50
  
lookAndSay :: String -> String
lookAndSay xs =
  concat [ show (length g) ++ [head g] | g <- group xs ]
