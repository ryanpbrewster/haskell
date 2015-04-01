modifiedFibSequence :: Integer -> Integer -> [Integer]
modifiedFibSequence a b = 
    let fibs = a : b : zipWith nextElement (tail fibs) (fibs)
    in fibs
  where nextElement tnp1 tn = tnp1^2 + tn

main = do
    [a, b, n] <- fmap (map read . words) getLine :: IO [Integer]
    print $ (modifiedFibSequence a b) !! (fromInteger (n-1))
