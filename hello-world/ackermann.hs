-- ackermann.hs
-- The Ackermann function

-- a m n
a 0 n = n+1
a m 0 = a (m-1) 1
a m n = a (m-1) (a m (n-1))

-- slightly optimized
a' 1 n = n+2
a' 2 n = 2*n+3
a' 3 n = 2^(n+3)-3
a' 0 n = n+1
a' m 0 = a' (m-1) 1
a' m n = a' (m-1) (a' m (n-1))
