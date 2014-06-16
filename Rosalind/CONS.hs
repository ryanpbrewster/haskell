import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (count)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.List (intercalate, maximumBy, transpose)
import Data.Ord (comparing)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let fastas = parseFASTAs txt
                       pm = makeProfileMatrix $ map getDNA fastas
                       cs = consensusString pm
                   in showDNA cs ++ "\n" ++ showProfileMatrix pm

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

data ProfileMatrix = ProfileMatrix (A.Array (Int,Int) Int) deriving (Show, Eq, Ord)
showProfileMatrix :: ProfileMatrix -> String
showProfileMatrix (ProfileMatrix pm) =
    let ((1,1), (4,n)) = A.bounds pm
    in unlines [ showNCL ncl ++ ": " ++ 
                   intercalate " " [ show $ pm A.! (i,j) | j <- [1..n] ]
                   | (i,ncl) <- zip [1..4] alphabet ]

{- Solution -}
count e xs = length $ filter (==e) xs


makeProfileMatrix :: [DNA] -> ProfileMatrix
makeProfileMatrix dnas =
    let n = length $ getNCLs $ head dnas -- length of the DNA strings
        bds = ((1,1), (4,n)) -- bounds
        nclm = transpose $ map getNCLs dnas -- base pair matrix
        pm = [ ((i,j), count ncl col) | (i,ncl) <- zip [1..4] alphabet
                                     , (j,col) <- zip [1..n] nclm ]
    in ProfileMatrix (A.array bds pm)

consensusString :: ProfileMatrix -> DNA
consensusString (ProfileMatrix pm) =
    let ((1,1),(4,n)) = A.bounds pm
        ncl_idxs = [ maximumBy (comparing (\i -> pm A.! (i,j))) [1..4] | j <- [1..n] ]
        ncl_map = M.fromList $ zip [1..4] alphabet
    in DNA $ map (ncl_map M.!) ncl_idxs


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
