module Rosalind.Parsing
( parseFASTA
, parseFASTAs
) where

import Rosalind.Structures
import Text.ParserCombinators.Parsec

parseFASTAs :: String -> [FASTA]
parseFASTAs input = case parse pFASTAs "" input of
    Right v -> v
    Left _  -> error $ "Improperly formatted input:" ++ show input

parseFASTA :: String -> FASTA
parseFASTA input = case parse pFASTA "" input of
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
