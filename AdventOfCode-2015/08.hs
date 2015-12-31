-- 08.hs
import Data.Char (chr, digitToInt)

main = do
  raw_strings <- fmap lines (readFile "08.input")
  let raw_length = sum $ map length raw_strings
  let parsed_strings = map (parseString . init . tail) raw_strings
  let parsed_length = sum $ map length parsed_strings
  print $ raw_length - parsed_length
  let encoded_strings = map ((\s -> "\"" ++ s ++ "\"") . encodeString) raw_strings
  let encoded_length = sum $ map length encoded_strings
  print $ encoded_length - raw_length

parseString :: String -> String
parseString "" = ""
parseString ('\\':'\\':r) = '\\' : parseString r
parseString ('\\':'"':r) = '"' : parseString r
parseString ('\\':'x':a:b:r) = chr (16 * digitToInt a + digitToInt b) : parseString r
parseString (v:str) = v : parseString str

encodeString :: String -> String
encodeString "" = ""
encodeString ('\\':r) = "\\\\" ++ encodeString r
encodeString ('"':r) = "\\\"" ++ encodeString r
encodeString (v:r) = v : encodeString r
