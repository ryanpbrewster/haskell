-- 013.hs


main = do filetext <- readFile "013.in"
          let ans = sum $ map read $ lines filetext
          putStrLn $ take 10 $ show ans
