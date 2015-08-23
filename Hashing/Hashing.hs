import Data.Digest.Murmur64
import Data.Bits
import Data.Word

data UserT = User { userId :: Int
                 } deriving (Show, Eq, Ord)
newUsers n = map User [1..n]

instance Hashable64 UserT where
  hash64Add u = hash64AddInt $ userId u

hashes :: (Hashable64 k) => k -> [Int]
hashes x = [ fromIntegral $ asWord64 $ hash64AddInt i $ hash64 x | i <- [1..5] ]

data BloomT a = Bloom Word64

empty :: BloomT a
empty = Bloom 0

instance Show (BloomT a) where
  show (Bloom bl) = pprint bl

bloomKey :: (Hashable64 k) => k -> Word64
bloomKey x = foldl setBit 0 [ (h `mod` 64) | h <- hashes x ]

insert :: (Hashable64 k) => k -> BloomT k -> BloomT k
insert x (Bloom bl) =
  Bloom $ bl .|. (bloomKey x)

insertAll :: (Hashable64 k) => [k] -> BloomT k -> BloomT k
insertAll xs bloom =
  foldl (flip insert) bloom xs

contains :: (Hashable64 k) => BloomT k -> k -> Bool
(Bloom bl) `contains` x = (bl `xor` (bloomKey x)) .|. bl == bl

pprint :: Word64 -> String
pprint x = [ if testBit x i then '1' else '0' | i <- [63, 62..0] ]
