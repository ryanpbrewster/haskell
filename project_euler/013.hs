-- Project Euler problem 13
-- Find the first ten digits of the sum of the numbers in 013.in


problem013 filename = do text <- readFile filename
                         let nums = map read $ lines text
                         return $ take 10 $ show $ sum nums

main = do ans <- problem013 "013.in"
          print ans
