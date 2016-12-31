import Data.Char (isAlpha, isLower, isUpper, chr, ord)

encrypt :: Int -> String -> String
encrypt shift = map $ shiftCharBy shift
decrypt shift = map $ shiftCharBy (-shift)

shiftCharBy shift ch
    | not (isAlpha ch) = ch
    | otherwise = let base = if isUpper ch then ord 'A' else ord 'a'
                      offset = ord ch - base + shift
                  in chr (base + offset `mod` 26)
