import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = txt
