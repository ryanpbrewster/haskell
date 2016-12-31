-- queue.hs
{-
 - Example of how to implement a Queue using two lists.
 - Pretty clever, I guess. Amortized O(1) performance for both enqueue and
 - dequeue
 -}

module Queue where 

data Queue a = Queue [a] [a] deriving (Show)

empty = Queue [] []

enqueue :: Queue a -> a -> Queue a
enqueue (Queue back front) v = Queue (v:back) front

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue [] []) = (Nothing, Queue [] [])
dequeue (Queue back (f:rest)) = (Just f, Queue back rest)
dequeue (Queue back []) = dequeue (Queue [] (reverse back))
