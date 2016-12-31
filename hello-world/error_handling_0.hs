-- error_handling_0.hs
{-
Trying to figure out how to write idiomatic code where a lot of
things can go wrong. This example is "Redeem a coupon code"

There are many things that need to happen to successfully redeem a coupon code
  1. The code must exist
  2. The code must be applicable
    a. Some codes are single-use, so they may have been burned
    b. Some codes have pre-reqs (e.g. only users >18 years old, or new users)
  3. The user must have permission to redeem credit codes
-}

import qualified Data.List as L
import qualified Data.Map as M


data Status = Okay | Burned deriving (Show, Eq, Ord)

data Code = Code String deriving (Show, Eq, Ord)

data CreditCodeInfo = CreditCodeInfo Code Status deriving (Show, Eq, Ord)
burned (CreditCodeInfo c _) = CreditCodeInfo c Burned

newCode str = CreditCodeInfo (normalize str) Okay
  where normalize x = Code x -- TODO(ryan): actually make this normalize

data Name = Name { firstName :: String, lastName :: String } deriving (Eq, Ord)
instance Show Name where
  show (Name fn ln) = "Name: (" ++ fn ++ ", " ++ ln ++ ")"

data User = User { age :: Int
                 , name :: Name
                 } deriving (Show, Eq)


c_MIN_APPLY_AGE = 18
-- This is the dumb "pure" way that I know how to do
-- I'm like 50% sure that there's a smart "ST" monad thing you can do here
applyCode :: Database -> User -> Code -> Maybe Database
applyCode db user code = do
  _ <- db `find` code
  _ <- if (age user) >= c_MIN_APPLY_AGE then Just "okay" else Nothing
  db `burn` code

data Database = Database (M.Map Code CreditCodeInfo) deriving (Show)
find :: Database -> Code -> Maybe CreditCodeInfo
find (Database code_infos) code =
  M.lookup code code_infos

burn :: Database -> Code -> Maybe Database
burn db@(Database code_infos) code = do
  v <- db `find` code
  return $ Database (M.insert code (burned v) code_infos)


-- TESTING DATA
test :: (Database, User, Code)
test = ( Database (M.singleton (Code "wapiti") (newCode "wapiti"))
       , User 19 (Name "a" "b")
       , Code "wapiti"
       )
