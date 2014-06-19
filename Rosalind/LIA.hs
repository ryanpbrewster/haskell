-- LIA.hs
{-
 - Problem
 - Given: Two positive integers k (k≤7) and N (N≤2k). In this problem, we begin
 - with Tom, who in the 0th generation has genotype Aa Bb. Tom has two children
 - in the 1st generation, each of whom has two children, and so on. Each
 - organism always mates with an organism having genotype Aa Bb.
 -
 - Return: The probability that at least N Aa Bb organisms will belong to the
 - k-th generation of Tom's family tree (don't count the Aa Bb mates at each
 - level). Assume that Mendel's second law holds for the factors.
 -
 - Sample Dataset
 -     2 1
 - Sample Output
 -     0.684
 -}
{-
 - The chance of having the genotype "Aa" is 1/2 for any individual
 - organism, no matter what generation. Same thing for "Bb".
 -
 - Thus, any individual organism has a 1/4 chance of having the desired
 - genotype. The chance of having c, the number of organisms with "Aa Bb",
 - be at least N organisms is then
 -     P(c >= N) = Sum[ P(c == i), {i,N,2^k} ]
 -               = Sum[ Binomial[2^k,i] * (1/4)^i * (3/4)^(2^k-i), {i,N,2^k} ]
 -}

import System.Environment (getArgs)
main = do
    args <- getArgs
    let [k,n] = map read args
    print $ p k n

fact n = product [1..n]
binomial n k = (fact n) `quot` ((fact k) * (fact (n-k)))

p :: Integer -> Integer -> Double
p k n = let t = 2^k
        in sum [ (fromIntegral $ binomial t i) * (0.25^i) * (0.75^(t-i)) | i <- [n..t]]

