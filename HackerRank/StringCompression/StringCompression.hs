import Data.List( group )

-- compress
compress :: String -> String
compress str = concat $ map abbreviate $ group str

-- abbreviate "aaa" == "a3"
-- abbreviate "a" == "a"
abbreviate :: [Char] -> String
abbreviate [ch] = [ch]
abbreviate chs = head chs : show (length chs)

main = do
    str <- getLine
    putStrLn $ compress str
