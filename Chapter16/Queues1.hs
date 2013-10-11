-------------------------------------------------------------------------
--  
--         Queues1.hs
--  
--         An abstract data type of queues, implemented as a list, with
--         new elements added at the end of the list.
-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------


module Queues1 
  ( Queue , 
    emptyQ ,       --  Queue a
    isEmptyQ ,     --  Queue a -> Bool 
    addQ ,         --  a -> Queue a -> Queue a
    remQ           --  Queue a -> (  a , Queue a )
   ) where 

newtype Queue a = Queue [a]
--  
emptyQ :: Queue a

emptyQ = Queue []

isEmptyQ :: Queue a -> Bool

isEmptyQ (Queue []) = True
isEmptyQ _       = False

addQ   :: a -> Queue a -> Queue a

addQ x (Queue xs) = Queue (xs++[x])

remQ   :: Queue a -> (  a , Queue a )

remQ q@(Queue xs)
  | not (isEmptyQ q)   = (head xs , Queue (tail xs))
  | otherwise          = error "remQ"

-- 16.11
-- Unique queue
-- The only difference from queue defined in 
-- Queues1.hs is in addQ function
-- This function must forbid the insertion of duplicate elements.
-- The question is: How should addQ behave when trying to add
-- a duplicate element to the queue?
-- Possible strategies are:
-- 1. addQ throws an error
-- 2. addQ doesn't add the element without providing any clue to the user
-- In this implementation we choose strategy nr. 2

addQ' :: Eq a => a -> Queue a -> Queue a
addQ' y (Queue xs)
  | elem y xs   = Queue xs
  | otherwise   = Queue (xs ++ [y])

