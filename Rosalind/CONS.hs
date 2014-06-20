import System.Environment (getArgs)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.List (intercalate, maximumBy, transpose)
import Data.Ord (comparing)
import Rosalind.Structures
import Rosalind.Parsing

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let fastas = parseNucFASTAs txt
                       pm = makeProfileMatrix $ map getSequence fastas
                       cs = consensusString pm
                   in show cs ++ "\n" ++ showProfileMatrix pm

{--------------------}
{- Data definitions -}
{--------------------}
data ProfileMatrix = ProfileMatrix (A.Array (Int,Int) Int) deriving (Show, Eq, Ord)
showProfileMatrix :: ProfileMatrix -> String
showProfileMatrix (ProfileMatrix pm) =
    let ((1,1), (4,n)) = A.bounds pm
    in unlines [ show ncl ++ ": " ++ 
                   intercalate " " [ show $ pm A.! (i,j) | j <- [1..n] ]
                   | (i,ncl) <- zip [1..4] allNucleotides ]

{- Solution -}
count :: Eq a => a -> [a] -> Int
count e xs = length $ filter (==e) xs


allNucleotides = map newNucleotide "ACGT"

makeProfileMatrix :: [BioSequence] -> ProfileMatrix
makeProfileMatrix nuc_seqs =
    let n = length $ getNucs $ head nuc_seqs -- length of the DNA strings
        bds = ((1,1), (4,n)) -- bounds
        nclm = transpose $ map getNucs nuc_seqs
        pm = [ ((i,j), count ncl col) | (i,ncl) <- zip [1..4] allNucleotides
                                      , (j,col) <- zip [1..n] nclm ]
    in ProfileMatrix (A.array bds pm)

consensusString :: ProfileMatrix -> BioSequence
consensusString (ProfileMatrix pm) =
    let ((1,1),(4,n)) = A.bounds pm
        ncl_idxs = [ maximumBy (comparing (\i -> pm A.! (i,j))) [1..4] | j <- [1..n] ]
        ncl_map = M.fromList $ zip [1..4] allNucleotides
    in NucleotideSequence $ map (ncl_map M.!) ncl_idxs
