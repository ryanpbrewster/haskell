-- egg k e
egg 0 e = 0
egg k 1 = k
egg k e = minimum [ 1 + max eggdies egglives | n <- [1..k]
                                             , let eggdies  = egg (n-1) (e-1)
                                             , let egglives = egg (k-n)  e  ]

eggs = let zero = repeat 0
           ans = zero : [ 0:k:[ optimalChoice ans k e | e <- [2..]] | k <- [1..]]
       in ans

optimalChoice ans k e = 1 + minimum [ max eggdies egglives
                                        | n <- [1..k]
                                        , let eggdies  = ans !! (n-1) !! (e-1)
                                        , let egglives = ans !! (k-n) !!  e  ]
