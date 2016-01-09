-- 12.hs
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Scientific

main = do
  inp <- B.getContents
  case eitherDecode inp :: Either String Value of
    Left err -> putStrLn err
    Right v -> do
      print $ sumNumbers v
      print $ sumNumbers $ dropRedObjects v

sumNumbers :: Value -> Scientific
sumNumbers (Number x) = x
sumNumbers (Array xs) = sum $ fmap sumNumbers xs
sumNumbers o@(Object xs) = sum $ fmap (sumNumbers . snd) $ HM.toList xs
sumNumbers _ = 0

dropRedObjects :: Value -> Value
dropRedObjects (Object xs)
  | any (== String "red") (HM.elems xs) = Object (HM.empty)
  | otherwise                           = Object $ HM.map dropRedObjects xs
dropRedObjects (Array xs) = Array $ fmap dropRedObjects xs
dropRedObjects v = v
