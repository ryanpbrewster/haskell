-- balanced_smileys.hs
{-
 - Your friend John uses a lot of emoticons when you talk to him on Messenger.
 - In addition to being a person who likes to express himself through
 - emoticons, he hates unbalanced parenthesis so much that it makes him go :(.
 -
 - Sometimes he puts emoticons within parentheses, and you find it hard to tell
 - if a parenthesis really is a parenthesis or part of an emoticon. A message
 - has balanced parentheses if it consists of one of the following:
 -
 -     * An empty string ""
 -     * One or more of the following characters: 'a' to 'z', ' ' (a space) or
 -       ':' (a colon)
 -     * An open parenthesis '(', followed by a message with balanced parentheses,
 -       followed by a close parenthesis ')'.
 -     * A message with balanced parentheses followed by another message with
 -       balanced parentheses.
 -     * A smiley face ":)" or a frowny face ":("
 -
 - Write a program that determines if there is a way to interpret his message
 - while leaving the parentheses balanced.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains a message that you got from John. e.g.
 -
 - :((
 - i am sick today (:()
 - (:)
 - hacker cup: started :):)
 - )(
 -
 - Output sample:
 -
 - Print out the string "YES"/"NO"(all quotes for clarity only) stating whether
 - or not it is possible that the message had balanced parentheses. e.g.
 -
 - NO
 - YES
 - YES
 - YES
 - NO
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = lines txt
                       anss = map possiblyBalanced inputs
                       outputs = [ if a then "YES" else "NO" | a <- anss ]
                   in unlines outputs

possiblyBalanced s = possiblyBalanced' s 0 0
    -- possiblyBalanced' s <min_left_parens> <max_left_parens>
    where possiblyBalanced' "" lo _  = lo == 0 -- at the end, it must balance
          possiblyBalanced' (')':s) _ 0  = False -- if there are too many ), that's bad
          possiblyBalanced' ('(':s)     lo hi = possiblyBalanced' s      (lo+1)    (hi+1)
          possiblyBalanced' (')':s)     lo hi = possiblyBalanced' s (max (lo-1) 0) (hi-1)
          possiblyBalanced' (':':'(':s) lo hi = possiblyBalanced' s       lo       (hi+1)
          possiblyBalanced' (':':')':s) lo hi = possiblyBalanced' s (max (lo-1) 0)  hi
          possiblyBalanced' (_:s)       lo hi = possiblyBalanced' s       lo        hi
