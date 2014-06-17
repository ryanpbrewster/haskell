-- LCSM.hs
{-
 - Problem
 -
 - A common substring of a collection of strings is a substring of every member of the collection. We say that a common substring is a longest common substring if there does not exist a longer common substring. For example, "CG" is a common substring of "ACGTACGT" and "AACCGGTATA", but it is not as long as possible; in this case, "GTA" is a longest common substring of "ACGTACGT" and "AACCGTATA".
 -
 - Note that the longest common substring is not necessarily unique; for a simple example, "AA" and "CC" are both longest common substrings of "AACC" and "CCAA".
 -
 - Given: A collection of k (k≤100) DNA strings of length at most 1 kbp each in FASTA format.
 -
 - Return: A longest common substring of the collection. (If multiple solutions exist, you may return any single solution.)
 - Sample Dataset
 -
 - >Rosalind_1
 - GATTACA
 - >Rosalind_2
 - TAGACCA
 - >Rosalind_3
 - ATACA
 -
 - Sample Output
 -
 - AC
 -}
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (count)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.List (intercalate, maximumBy, transpose, isInfixOf, nub)
import Data.Ord (comparing)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let fastas = parseFASTAs txt
                       anss = longestCommonSubstrings $ map (getNCLs.getDNA) fastas
                   in showDNA $ DNA (head anss)

{--------------------}
{- Data definitions -}
{--------------------}
data FASTA = FASTA { getID :: ID
                   , getDNA :: DNA
                   } deriving (Show, Eq, Ord)

data ID = ID String deriving (Show, Eq, Ord)
showID (ID id) = id

data DNA = DNA { getNCLs :: [Nucleotide] } deriving (Show, Eq, Ord)
showDNA (DNA ncls) = concat $ map showNCL ncls

data Nucleotide = Nucleotide Char deriving (Show, Eq, Ord)
showNCL (Nucleotide ncl) = [ncl]
alphabet = map Nucleotide "ACGT"

{- Solution -}
expandOption opt = [ a:opt | a <- alphabet ] ++ [ opt ++ [a] | a <- alphabet ]
longestCommonSubstrings xss = lcss [[]]
    where lcss opts = let maybes = nub $ concat $ map expandOption opts
                          opts' = filter (\m -> all (m `isInfixOf`) xss) maybes
                      in if null opts' then opts else lcss opts'


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
    return $ DNA $ map Nucleotide $ filter (/='\n') dna_str
