-- cash_register.hs
{-
 -  The goal of this challenge is to design a cash register program. You will
 -  be given two float numbers. The first is the purchase price (PP) of the
 -  item. The second is the cash (CH) given by the customer. Your register
 -  currently has the following bills/coins within it:
 -
 -  'PENNY': .01,
 -  'NICKEL': .05,
 -  'DIME': .10,
 -  'QUARTER': .25,
 -  'HALF DOLLAR': .50,
 -  'ONE': 1.00,
 -  'TWO': 2.00,
 -  'FIVE': 5.00,
 -  'TEN': 10.00,
 -  'TWENTY': 20.00,
 -  'FIFTY': 50.00,
 -  'ONE HUNDRED': 100.00
 -
 -  The aim of the program is to calculate the change that has to be returned
 -  to the customer.
 -
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename. The
 -  input file contains several lines. Each line is one test case. Each line
 -  contains two numbers which are separated by a semicolon. The first is the
 -  Purchase price (PP) and the second is the cash(CH) given by the customer.
 -  eg.
 -
 -  15.94;16.00
 -  17;16
 -  35;35
 -  45;50
 -
 -  Output sample:
 -
 -  For each set of input produce a single line of output which is the change
 -  to be returned to the customer. In case the CH < PP, print out ERROR. If CH
 -  == PP, print out ZERO. For all other cases print the amount that needs to
 -  be returned, in terms of the currency values provided. The output should be
 -  sorted in highest-to-lowest order (DIME,NICKEL,PENNY). eg.
 -
 -  NICKEL,PENNY
 -  ERROR
 -  ZERO
 -  FIVE
 -}

{-
 - Luckily, this set of denominations is succeptible to the greedy approach,
 - where we just take the biggest chunk of currency that we can and move from
 - there.
 -}




import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe (fromJust)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let lns = [ map read $ wordsBy (==';') ln | ln <- lines txt ]
                       inputs = [ round $ 100*(cash-cost) | [cost,cash] <- lns ]
                       outputs = map cashRegisterInstructions inputs
                   in unlines outputs

cashRegisterInstructions n | n < 0  = "ERROR"
                           | n == 0 = "ZERO"
                           | otherwise =
    let change = makeChange n cash_register_coins
        coin_names = map coinName change
    in intercalate "," coin_names

makeChange :: Integer -> [Integer] -> [Integer]
makeChange 0 _ = []
makeChange n cs@(c:cs') | c >  n =    makeChange n     cs'
                        | c <= n = c:(makeChange (n-c) cs)


cash_register_coins = reverse [1, 5, 10, 25, 50, 100, 200, 500, 1000, 2000, 5000, 10000]
cash_register_names = Map.fromList [ (1,     "PENNY"      )
                                   , (5,     "NICKEL"     )
                                   , (10,    "DIME"       )
                                   , (25,    "QUARTER"    )
                                   , (50,    "HALF DOLLAR")
                                   , (100,   "ONE"        )
                                   , (200,   "TWO"        )
                                   , (500,   "FIVE"       )
                                   , (1000,  "TEN"        )
                                   , (2000,  "TWENTY"     )
                                   , (5000,  "FIFTY"      )
                                   , (10000, "ONE HUNDRED")
                                   ]
coinName :: Integer -> String
coinName n = fromJust $ Map.lookup n cash_register_names
