-- 185.hs
{-
 - The game Number Mind is a variant of the well known game Master Mind.
 -
 - Instead of coloured pegs, you have to guess a secret sequence of digits.
 - After each guess you're only told in how many places you've guessed the
 - correct digit. So, if the sequence was 1234 and you guessed 2036, you'd be
 - told that you have one correct digit; however, you would NOT be told that
 - you also have another digit in the wrong place.
 -
 - For instance, given the following guesses for a 5-digit secret sequence,
 -
 - 90342 ;2 correct
 - 70794 ;0 correct
 - 39458 ;2 correct
 - 34109 ;1 correct
 - 51545 ;2 correct
 - 12531 ;1 correct
 -
 - The correct sequence 39542 is unique.
 -
 - Based on the following guesses,
 -
 - 5616185650518293 ;2 correct
 - 3847439647293047 ;1 correct
 - 5855462940810587 ;3 correct
 - 9742855507068353 ;3 correct
 - 4296849643607543 ;3 correct
 - 3174248439465858 ;1 correct
 - 4513559094146117 ;2 correct
 - 7890971548908067 ;3 correct
 - 8157356344118483 ;1 correct
 - 2615250744386899 ;2 correct
 - 8690095851526254 ;3 correct
 - 6375711915077050 ;1 correct
 - 6913859173121360 ;1 correct
 - 6442889055042768 ;2 correct
 - 2321386104303845 ;0 correct
 - 2326509471271448 ;2 correct
 - 5251583379644322 ;2 correct
 - 1748270476758276 ;3 correct
 - 4895722652190306 ;1 correct
 - 3041631117224635 ;3 correct
 - 1841236454324589 ;3 correct
 - 2659862637316867 ;2 correct
 -
 - Find the unique 16-digit secret sequence.
 -}


testg = "90342":
        "70794":
        "39458":
        "34109":
        "51545":
        "12531":
        []

testa = [2,0,2,1,2,1]
tests = [3,9,5,4,2]

(bigtestg, bigtesta) = unzip (("5616185650518293", 2):
                              ("3847439647293047", 1):
                              ("5855462940810587", 3):
                              ("9742855507068353", 3):
                              ("4296849643607543", 3):
                              ("3174248439465858", 1):
                              ("4513559094146117", 2):
                              ("7890971548908067", 3):
                              ("8157356344118483", 1):
                              ("2615250744386899", 2):
                              ("8690095851526254", 3):
                              ("6375711915077050", 1):
                              ("6913859173121360", 1):
                              ("6442889055042768", 2):
                              ("2321386104303845", 0):
                              ("2326509471271448", 2):
                              ("5251583379644322", 2):
                              ("1748270476758276", 3):
                              ("4895722652190306", 1):
                              ("3041631117224635", 3):
                              ("1841236454324589", 3):
                              ("2659862637316867", 2):
                              [])



delta i j | i == j    = 1
          | otherwise = 0

guessSequence guesses anss
    | null (head guesses) = if all (==0) anss then [[]] else []
    | otherwise =
        let ans_front_pairs = zip anss (map head guesses)
            guesses' = map tail guesses
            opts = [ cur:seq | cur <- ['0'..'9']
                             , let anss' = [ ans - delta cur f | (ans,f) <- ans_front_pairs ]
                             , all (>=0) anss'
                             , seq <- guessSequence guesses' anss' ]
        in opts

main = print $ guessSequence bigtestg bigtesta
