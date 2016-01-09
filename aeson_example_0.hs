import Data.Aeson
import qualified Data.ByteString.Lazy as B

main = do
  txt <- B.getContents
  print $ (decode txt :: Maybe Value)
