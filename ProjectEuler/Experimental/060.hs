-- 060.hs
{-
 - The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
 - primes and concatenating them in any order the result will always be prime.
 - For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of
 - these four primes, 792, represents the lowest sum for a set of four primes
 - with this property.
 -
 - Find the lowest sum for a set of five primes for which any two primes
 - concatenate to produce another prime.
 -}

import qualified ProjectEuler.Prime as Prime
import ProjectEuler.Util (mergeInf, sublists)

data CCPrimes = CCP { getTotal :: Integer
                    , getPrimes :: [Integer]
                    } deriving (Eq, Ord)
instance Show CCPrimes where
    show (CCP t ps) = "sum(" ++ show ps ++ ") = " ++ show t

empty :: CCPrimes
empty = CCP 0 []

insert :: Integer -> CCPrimes -> CCPrimes
insert p (CCP tot ps) = CCP (tot+p) (p:ps)

legitFront :: CCPrimes -> Bool
legitFront (CCP _ (p:ps)) = let at_front = [ concatInts p  p' | p' <- ps ]
                                at_back  = [ concatInts p' p  | p' <- ps ]
                        in all Prime.test $ at_front ++ at_back

newCCPs (CCP _ []) = [ insert p' empty | p' <- Prime.primes ]
newCCPs ccp@(CCP _ (p:_)) = [ insert p' ccp | p' <- dropWhile (<=p) Prime.primes ]

concatInts a b = (prefactor b)*a + b
prefactor 0 = 10
prefactor n = prefactor' n
    where prefactor' 0 = 1
          prefactor' n = 10 * prefactor' (n `quot` 10)

findCCPs 0 = [empty]
findCCPs k = filter legitFront $ mergeInf $ map newCCPs $ findCCPs (k-1)

solveProblem = getTotal $ head $ findCCPs 4

main = print solveProblem
