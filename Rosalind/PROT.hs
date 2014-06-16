-- PROT.hs
{-
 - Problem
 -
 - The 20 commonly occurring amino acids are abbreviated by using 20 letters
 - from the English alphabet (all letters except for B, J, O, U, X, and Z).
 - Protein strings are constructed from these 20 symbols. Henceforth, the term
 - genetic string will incorporate protein strings along with DNA strings and
 - RNA strings.
 -
 - The RNA codon table dictates the details regarding the encoding of specific
 - codons into the amino acid alphabet.
 -
 - Given: An RNA string s corresponding to a strand of mRNA (of length at most 10 kbp).
 -
 - Return: The protein string encoded by s.
 - Sample Dataset
 -     AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
 - Sample Output
 -     MAMAPRTEINSTRING
 -}

import System.Environment (getArgs)
import qualified Data.Map as M

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let inp = concat $ lines txt
                       protein = constructProteinString inp
                   in showProtein protein

data AminoAcid = AA Char
showProtein :: [AminoAcid] -> String
showProtein aas = [ ch | AA ch <- aas ]

data CodonInstruction = CI { toAA :: AminoAcid } | Stop
stop Stop = True
stop _    = False

codon_map = M.fromList [ ("UUU",CI $ AA 'F'), ("CUU",CI $ AA 'L'), ("AUU",CI $ AA 'I'), ("GUU",CI $ AA 'V')
                       , ("UUC",CI $ AA 'F'), ("CUC",CI $ AA 'L'), ("AUC",CI $ AA 'I'), ("GUC",CI $ AA 'V')
                       , ("UUA",CI $ AA 'L'), ("CUA",CI $ AA 'L'), ("AUA",CI $ AA 'I'), ("GUA",CI $ AA 'V')
                       , ("UUG",CI $ AA 'L'), ("CUG",CI $ AA 'L'), ("AUG",CI $ AA 'M'), ("GUG",CI $ AA 'V')
                       , ("UCU",CI $ AA 'S'), ("CCU",CI $ AA 'P'), ("ACU",CI $ AA 'T'), ("GCU",CI $ AA 'A')
                       , ("UCC",CI $ AA 'S'), ("CCC",CI $ AA 'P'), ("ACC",CI $ AA 'T'), ("GCC",CI $ AA 'A')
                       , ("UCA",CI $ AA 'S'), ("CCA",CI $ AA 'P'), ("ACA",CI $ AA 'T'), ("GCA",CI $ AA 'A')
                       , ("UCG",CI $ AA 'S'), ("CCG",CI $ AA 'P'), ("ACG",CI $ AA 'T'), ("GCG",CI $ AA 'A')
                       , ("UAU",CI $ AA 'Y'), ("CAU",CI $ AA 'H'), ("AAU",CI $ AA 'N'), ("GAU",CI $ AA 'D')
                       , ("UAC",CI $ AA 'Y'), ("CAC",CI $ AA 'H'), ("AAC",CI $ AA 'N'), ("GAC",CI $ AA 'D')
                       , ("UAA",Stop),        ("CAA",CI $ AA 'Q'), ("AAA",CI $ AA 'K'), ("GAA",CI $ AA 'E')
                       , ("UAG",Stop),        ("CAG",CI $ AA 'Q'), ("AAG",CI $ AA 'K'), ("GAG",CI $ AA 'E')
                       , ("UGU",CI $ AA 'C'), ("CGU",CI $ AA 'R'), ("AGU",CI $ AA 'S'), ("GGU",CI $ AA 'G')
                       , ("UGC",CI $ AA 'C'), ("CGC",CI $ AA 'R'), ("AGC",CI $ AA 'S'), ("GGC",CI $ AA 'G')
                       , ("UGA",Stop),        ("CGA",CI $ AA 'R'), ("AGA",CI $ AA 'R'), ("GGA",CI $ AA 'G')
                       , ("UGG",CI $ AA 'W'), ("CGG",CI $ AA 'R'), ("AGG",CI $ AA 'R'), ("GGG",CI $ AA 'G')
                       ]


chunksOf _ [] = []
chunksOf k xs = let (chk, xs') = splitAt k xs in chk : chunksOf k xs'

constructProteinString bps = let codons = chunksOf 3 bps
                                 instructions = map (codon_map M.!) codons
                             in map toAA $ takeWhile (not.stop) instructions
