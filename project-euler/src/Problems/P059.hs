module Problems.P059
  ( process
  ) where

import Data.Bits (xor)
{-
 - Each character on a computer is assigned a unique code and the preferred
 - standard is ASCII (American Standard Code for Information Interchange). For
 - example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
 -
 - A modern encryption method is to take a text file, convert the bytes to
 - ASCII, then XOR each byte with a given value, taken from a secret key. The
 - advantage with the XOR function is that using the same encryption key on the
 - cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107
 - XOR 42 = 65.
 -
 - For unbreakable encryption, the key is the same length as the plain text
 - message, and the key is made up of random bytes. The user would keep the
 - encrypted message and the encryption key in different locations, and without
 - both "halves", it is impossible to decrypt the message.
 -
 - Unfortunately, this method is impractical for most users, so the modified
 - method is to use a password as a key. If the password is shorter than the
 - message, which is likely, the key is repeated cyclically throughout the
 - message. The balance for this method is using a sufficiently long password
 - key for security, but short enough to be memorable.
 -
 - Your task has been made easy, as the encryption key consists of three lower
 - case characters. Using 059.in, a file containing the encrypted ASCII codes,
 - and the knowledge that the plain text must contain common English words,
 - decrypt the message and find the sum of the ASCII values in the original
 - text.
 -}
import qualified Data.Char as DC
import Data.List (maximumBy)

type FileContents = String

process :: FileContents -> String
process txt =
  let ciphertext' = map read $ lines txt
      ciphertext = map DC.chr ciphertext'
  in show $ solveProblem ciphertext

solveProblem ciphertext =
  let shortciphertext = take 100 ciphertext
      possible_keys = tuples 3 ['a' .. 'z']
      possible_plaintexts =
        [decipher shortciphertext key | key <- possible_keys]
      key = fst $ maximumBy metric $ zip possible_keys possible_plaintexts
      plaintext = decipher ciphertext key
  in sum $ map DC.ord plaintext

tuples 0 _ = [[]] -- only the empty tuple
tuples k xs = [x : t | t <- tuples (k - 1) xs, x <- xs]

decipher ciphertext key =
  let key' = map DC.ord $ cycle key
      ciphertext' = map DC.ord ciphertext
      plaintext' = zipWith xor key' ciphertext'
  in map DC.chr plaintext'

-- compare plaintext1 vs. plaintext2
metric kv1 kv2 = simpleFrequencyMetric (snd kv1) (snd kv2)

simpleFrequencyMetric pt1 pt2 =
  let score1 = sum $ map frequency pt1
      score2 = sum $ map frequency pt2
  in compare score1 score2

frequency 'e' = 12
frequency 't' = 9
frequency 'a' = 8
frequency 'i' = 8
frequency 'n' = 8
frequency 'o' = 8
frequency 's' = 8
frequency 'h' = 6
frequency 'r' = 6
frequency _ = 0
