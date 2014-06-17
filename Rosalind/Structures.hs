module Rosalind.Structures
where

data FASTA = FASTA { getID :: ID
                   , getDNA :: DNA
                   } deriving (Show, Eq, Ord)

data ID = ID String deriving (Show, Eq, Ord)
showID (ID id) = id

data DNA = DNA { getNCLs :: [Nucleotide] } deriving (Show, Eq, Ord)
showDNA (DNA ncls) = concat $ map showNCL ncls

makeDNA :: String -> DNA
makeDNA str = DNA $ map Nucleotide str

data Nucleotide = Nucleotide Char deriving (Show, Eq, Ord)
showNCL (Nucleotide ncl) = [ncl]

alphabet = map Nucleotide "ACGT"
