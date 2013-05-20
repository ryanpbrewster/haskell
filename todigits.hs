-- digits(n,b) :: Integer -> [Integer]

digits 0 _ = [0]
digits n b = reverse $ digits' n b

digits' 0 _ = []
digits' n b = let (q,r) = quotRem n b
              in r:(digits' q b)
