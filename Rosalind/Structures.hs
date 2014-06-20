module Rosalind.Structures
where

import Control.Exception (assert)
import qualified Data.Map as M

data FASTA = FASTA { getID :: ID
                   , getSequence :: BioSequence
                   } deriving (Show, Eq, Ord)

data ID = ID String deriving (Show, Eq, Ord)
showID (ID id) = id

data BioSequence = NucleotideSequence { getNucs :: [Nucleotide] }
                 | PeptideSequence { getPeps :: [Peptide] }
                 deriving (Show, Eq, Ord)

showSequence (NucleotideSequence ncls) = concat $ map showNucleotide ncls
showSequence (PeptideSequence peps) = concat $ map showPeptide peps

newNucleotideSequence str = NucleotideSequence $ map newNucleotide str
newPeptideSequence str = PeptideSequence $ map newPeptide str

data Nucleotide = Nucleotide Char deriving (Eq, Ord)
instance Show Nucleotide where show = showNucleotide
showNucleotide (Nucleotide ncl) = [ncl]

nucleotideAlphabet = "ACGTU"
newNucleotide chr = assert (chr `elem` nucleotideAlphabet) $ Nucleotide chr

data Peptide = Peptide Char deriving (Eq, Ord)
instance Show Peptide where show = showPeptide
showPeptide (Peptide ncl) = [ncl]

peptideAlphabet = "ACDEFGHIKLMNPQRSTVWY"
newPeptide chr = assert (chr `elem` peptideAlphabet) $ Peptide chr

data CodonInstruction = MakePeptide Peptide | Stop deriving (Show, Eq, Ord)

c_CODON_MAP = M.fromList $ zip ncl_seqs cis
    where seqs = ["UUU","CUU","AUU","GUU","UUC","CUC","AUC","GUC"
                 ,"UUA","CUA","AUA","GUA","UUG","CUG","AUG","GUG"
                 ,"UCU","CCU","ACU","GCU","UCC","CCC","ACC","GCC"
                 ,"UCA","CCA","ACA","GCA","UCG","CCG","ACG","GCG"
                 ,"UAU","CAU","AAU","GAU","UAC","CAC","AAC","GAC"
                 ,"CAA","AAA","GAA","CAG","AAG","GAG","UGU","CGU"
                 ,"AGU","GGU","UGC","CGC","AGC","GGC","CGA","AGA"
                 ,"GGA","UGG","CGG","AGG","GGG","UAA","UAG","UGA"
                 ]
          peptides = "FLIVFLIVLLIVLLMVSPTASPTASPTASPTAYHNDYHNDQKEQKECRSGCRSGRRGWRRG"
          ncl_seqs = [ map newNucleotide seq | seq <- seqs ]
          cis = [ MakePeptide (newPeptide chr) | chr <- peptides ] ++ replicate 3 Stop
