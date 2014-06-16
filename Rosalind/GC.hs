-- GC.hs
{-
 - Problem
 -
 - The GC-content of a DNA string is given by the percentage of symbols in the string that are 'C' or 'G'. For example, the GC-content of "AGCTATAG" is 37.5%. Note that the reverse complement of any DNA string has the same GC-content.
 -
 - DNA strings must be labeled when they are consolidated into a database. A commonly used method of string labeling is called FASTA format. In this format, the string is introduced by a line that begins with '>', followed by some labeling information. Subsequent lines contain the string itself; the first line to begin with '>' indicates the label of the next string.
 -
 - In Rosalind's implementation, a string in FASTA format will be labeled by
 - the ID "Rosalind_xxxx", where "xxxx" denotes a four-digit code between 0000
 - and 9999.
 -
 - Given: At most 10 DNA strings in FASTA format (of length at most 1 kbp each).
 -
 - Return: The ID of the string having the highest GC-content, followed by the
 - GC-content of that string. Rosalind allows for a default error of 0.001 in
 - all decimal answers unless otherwise stated; please see the note on absolute
 - error below.
 -
 - Sample Dataset
 -
 - >Rosalind_6404
 - CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
 - TCCCACTAATAATTCTGAGG
 - >Rosalind_5959
 - CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
 - ATATCCATTTGTCAGCAGACACGC
 - >Rosalind_0808
 - CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
 - TGGGAACCTGCGGGCAGTAGGTGGAAT
 -
 - Sample Output
 -
 - Rosalind_0808
 - 60.919540
 -}

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Text.Printf
import Data.Ord (comparing)
import Data.List (maximumBy)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let inps = parseFASTAs txt
                       ans = maximumBy (comparing (gcContent.getDNA)) inps
                       out = showAnswer ans
                   in out

showAnswer fsta@(FASTA id dna) = unlines [ showID id
                                       , printf "%.6f" (100 * gcContent dna) ]

{--------------------}
{- Data definitions -}
{--------------------}
data FASTA = FASTA { getID :: ID
                   , getDNA :: DNA
                   } deriving (Show, Eq, Ord)

data ID = ID String deriving (Show, Eq, Ord)
showID (ID id) = id

data DNA = DNA String deriving (Show, Eq, Ord)
showDNA (DNA dna) = dna

{---------------------}
{- Parsing Functions -}
{---------------------}
parseFASTAs :: String -> [FASTA]
parseFASTAs input = case parse pFASTAs "" input of
    Right v -> v
    Left _  -> error $ "Improperly formatted input:" ++ show input

pFASTAs = many pFASTA
pFASTA = do
    id <- pID
    dna <- pDNA
    return $ FASTA id dna

pID = do
    char '>'
    id <- many (noneOf "\n")
    return $ ID id
pDNA = do
    dna_str <- many (oneOf "ACGT\n")
    return $ DNA (filter (/='\n') dna_str)

{------------}
{- Solution -}
{------------}

gc :: Char -> Bool
gc bp = bp == 'G' || bp == 'C'

gcContent :: DNA -> Double
gcContent (DNA bps) =
    let gc_content = length $ filter gc bps
        total = length bps
    in (fromIntegral gc_content) / (fromIntegral total)
