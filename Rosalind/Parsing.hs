module Rosalind.Parsing
( parseNucFASTAs
, parsePepFASTAs
) where

import Rosalind.Structures
import Text.ParserCombinators.Parsec

parseNucFASTAs :: String -> [FASTA]
parseNucFASTAs input = case parse pNucFASTAs "" input of
    Right v -> v
    Left _  -> error $ "Improperly formatted input:" ++ show input

parsePepFASTAs :: String -> [FASTA]
parsePepFASTAs input = case parse pPepFASTAs "" input of
    Right v -> v
    Left _  -> error $ "Improperly formatted input:" ++ show input

pNucFASTAs = many pNucFASTA
pNucFASTA = do
    id <- pID
    seq <- pNucSeq
    return $ FASTA id seq

pPepFASTAs = many pNucFASTA
pPepFASTA = do
    id <- pID
    seq <- pPepSeq
    return $ FASTA id seq

pID = do
    char '>'
    id <- many (noneOf "\n")
    return $ ID id

pNucSeq = do
    str <- many (noneOf ">")
    return $ NucleotideSequence (map newNucleotide $ filter (/='\n') str)

pPepSeq = do
    str <- many (noneOf ">")
    return $ PeptideSequence (map newPeptide $ filter (/='\n') str)
