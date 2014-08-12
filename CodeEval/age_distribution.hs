-- age_distribution.hs
{-
 -  Age destribution
 -  Challenge Description:

 -  In this challenge, we are analyzing the ages of people to determine their status.

 -  If the age is from 0 to 2 the child should be with parents at home, print : 'Home'
 -  If the age is from 3 to 4 the child should visit preschool, print : 'Preschool'
 -  If the age is from 5 to 11 the child should visit elementary school, print : 'Elementary school'
 -  From 12 to 14: 'Middle school'
 -  From 15 to 18: 'High school'
 -  From 19 to 22: 'College'
 -  From 23 to 65: 'Work'
 -  From 66 to 100: 'Retirement'
 -  If the age of the person less than 0 or more than 100 - it might be an alien - print: "This program is for humans"
 -  Input sample:

 -  Your program should accept as its first argument a path to a filename. Each line of input contains one integer - age of the person:

 -  Output sample:

 -  For each line of input print out where the person is:
 -}

import System.Environment (getArgs)
main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = map read $ lines txt
                       anss = map ageToLocation inps
                   in unlines anss

ageToLocation :: Int -> String
ageToLocation x | x <    0 = "This program is for humans"
                | x <=   2 = "Home"
                | x <=   4 = "Preschool"
                | x <=  11 = "Elementary school"
                | x <=  14 = "Middle school"
                | x <=  18 = "High school"
                | x <=  22 = "College"
                | x <=  65 = "Work"
                | x <= 100 = "Retirement"
                | x >  100 = "This program is for humans"
