-- endianness.hs
{-
 - CHALLENGE DESCRIPTION:
 -
 - Write a program to print out the endianness of the system.
 -
 - INPUT SAMPLE:
 -
 - None
 -
 - OUTPUT SAMPLE:
 -
 - Print to stdout, the endianness, wheather it is LittleEndian or BigEndian.
 - e.g.
 -
 - BigEndian
 -}

import Data.Binary (decode)
import Data.Binary.Put (putWord16host, runPut)
import Data.Word (Word8)

main = do
    putStrLn solveProblem

solveProblem = if littleEndian then "LittleEndian" else "BigEndian"

littleEndian = let raw = decode $ runPut $ putWord16host 0x11 :: Word8
               in raw == 0x11

