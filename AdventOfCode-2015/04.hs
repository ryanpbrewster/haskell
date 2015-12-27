-- 04.hs
import Data.Hash.MD5
import Data.List (isPrefixOf)

main = do
  let secret = "bgvyzdsv"
  print $ head $ goodKeys secret ((replicate 5 '0') `isPrefixOf`)
  print $ head $ goodKeys secret ((replicate 6 '0') `isPrefixOf`)

goodKeys :: String -> (String -> Bool) -> [Int]
goodKeys secret pred = filter good [1..]
  where good i = pred $ md5s $ Str (secret ++ show i)
