-- MRNA.hs
{-
 - Given: A protein string of length at most 1000 aa.
 -
 - Return: The total number of different RNA strings from which the
 - protein could have been translated, modulo 1,000,000. (Don't neglect
 - the importance of the stop codon in protein translation.)
 -}

import Rosalind.Structures
import qualified Data.Map as M
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

c_MOD = 10^6
solveProblem txt = let inp = map newPeptide $ concat $ lines txt
                       ans = countRNAStrings inp `mod` c_MOD
                   in show ans

countRNAStrings :: [Peptide] -> Integer
countRNAStrings peps =
    let cil = Stop : map MakePeptide peps -- codon instruction list
        opts = map ciOptions cil -- options per codon instruction
    in product opts

ciOptions :: CodonInstruction -> Integer
ciOptions ci = c_CODON_OPT_MAP M.! ci

c_CODON_OPT_MAP :: M.Map CodonInstruction Integer
c_CODON_OPT_MAP = M.fromListWith (+) [ (ci,1) | ci <- M.elems c_CODON_MAP ]
