-- PRTM.hs
{-
 - In a weighted alphabet, every symbol is assigned a positive real number called a weight. A string formed from a weighted alphabet is called a weighted string, and its weight is equal to the sum of the weights of its symbols.
 -
 - The standard weight assigned to each member of the 20-symbol amino acid alphabet is the monoisotopic mass of the corresponding amino acid.
 -
 - Given: A protein string P of length at most 1000 aa.
 -
 - Return: The total weight of P. Consult the monoisotopic mass table.
 - Sample Dataset
 -
 - SKADYEK
 -
 - Sample Output
 -
 - 821.392
 -}

import qualified Data.Map as M
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let inps = lines txt
                       anss = map computeProteinMass inps
                       outs = map show anss
                   in unlines outs

mass_table = M.fromList [('A', 71.037110)
                        ,('C', 103.00919)
                        ,('D', 115.02694)
                        ,('E', 129.04259)
                        ,('F', 147.06841)
                        ,('G', 57.021460)
                        ,('H', 137.05891)
                        ,('I', 113.08406)
                        ,('K', 128.09496)
                        ,('L', 113.08406)
                        ,('M', 131.04049)
                        ,('N', 114.04293)
                        ,('P', 97.052760)
                        ,('Q', 128.05858)
                        ,('R', 156.10111)
                        ,('S', 87.032030)
                        ,('T', 101.04768)
                        ,('V', 99.068410)
                        ,('W', 186.07931)
                        ,('Y', 163.06333)]

computeProteinMass :: String -> Double
computeProteinMass = sum . map (mass_table M.!)
