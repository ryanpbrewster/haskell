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

solveProblem txt = let fastas = parseFASTAs txt
                       pm = makeProfileMatrix $ map getDNA fastas
                       cs = consensusString pm
                   in showDNA cs ++ "\n" ++ showProfileMatrix pm

{--------------------}
{- Data definitions -}
{--------------------}
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
