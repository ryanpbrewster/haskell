-- message_decoding.hs
{-
 - Credits: This challenge has appeared in a past ACM competition.
 -
 - Some message encoding schemes require that an encoded message be sent in two
 - parts. The first part, called the header, contains the characters of the
 - message. The second part contains a pattern that represents the message.You
 - must write a program that can decode messages under such a scheme.
 -
 - The heart of the encoding scheme for your program is a sequence of "key"
 - strings of 0's and 1's as follows:
 - 0,00,01,10,000,001,010,011,100,101,110,0000,0001,. . .,1011,1110,00000, . . .
 -
 - The first key in the sequence is of length 1, the next 3 are of length 2,
 - the next 7 of length 3, the next 15 of length 4, etc. If two adjacent keys
 - have the same length, the second can be obtained from the first by adding
 - 1 (base 2). Notice that there are no keys in the sequence that consist only
 - of 1's.
 -
 - The keys are mapped to the characters in the header in order. That is, the
 - first key (0) is mapped to the first character in the header, the second key
 - (00) to the second character in the header, the kth key is mapped to the kth
 - character in the header. For example, suppose the header is:
 -
 - AB#TANCnrtXc
 - Then 0 is mapped to A, 00 to B, 01 to #, 10 to T, 000 to A, ..., 110 to X,
 - and 0000 to c.
 -
 - The encoded message contains only 0's and 1's and possibly carriage returns,
 - which are to be ignored. The message is divided into segments. The first
 - 3 digits of a segment give the binary representation of the length of the
 - keys in the segment. For example, if the first 3 digits are 010, then the
 - remainder of the segment consists of keys of length 2 (00, 01, or 10). The
 - end of the segment is a string of 1's which is the same length as the length
 - of the keys in the segment. So a segment of keys of length 2 is terminated
 - by 11. The entire encoded message is terminated by 000 (which would signify
 - a segment in which the keys have length 0). The message is decoded by
 - translating the keys in the segments one-at-a-time into the header
 - characters to which they have been mapped.
 -
 - Input sample:
 -
 - The input file contains several data sets. Each data set consists of
 - a header and a message. These will all be on one line. The length of the
 - header is limited only by the fact that key strings have a maximum length of
 - 7 (111 in binary). If there are multiple copies of a character in a header,
 - then several keys will map to that character. The encoded message contains
 - only 0's and 1's, and it is a legitimate encoding according to the described
 - scheme. That is, the message segments begin with the 3-digit length sequence
 - and end with the appropriate sequence of 1's. The keys in any given segment
 - are all of the same length, and they all correspond to characters in the
 - header. The message is terminated by 000.
 -
 - Your program should accept the first argument as the filename and read the
 - contents of this file as the test data, according to the conditions
 - above.eg.
 -
 -     $#**\0100000101101100011100101000
 -
 - Output sample:
 -
 - For each data set, your program must write its decoded message on a separate
 - line. There should not be blank lines between messages.eg.
 -
 -     ##*\$
 -}



import System.Environment (getArgs)
import Data.Map (fromList, (!))
import Data.Char (digitToInt)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = [ break (`elem` "01") ln | ln <- lines txt ]
                       outputs = [ decodeString h str | (h, str) <- inputs ]
                   in unlines outputs

hasAZero = any (=='0')
binary_strings = "" : concat [ [s ++ "0", s ++ "1"] | s <- binary_strings ]
prefixes = filter hasAZero binary_strings

decodeString header ciphertext = let cipher = makeCipher header
                                 in decipher cipher ciphertext

makeCipher header = fromList $ zip prefixes header

decipher cipher "000" = "" -- end at "000"
decipher cipher str =
    let (len_s,rest) = splitAt 3 str
        len = binaryToInt len_s
    in decipherChunksOf cipher len rest

decipherChunksOf cipher len str =
    let (cnk, rest) = splitAt len str
    in if not (hasAZero cnk)
        then decipher cipher rest
        else (cipher ! cnk) : decipherChunksOf cipher len rest

binaryToInt str = binaryToInt' str 0
    where binaryToInt'   ""   acc = acc
          binaryToInt' (x:xs) acc = binaryToInt' xs (2*acc + digitToInt x)
