--------------------------------------
--- Ex 16.10                      ----
--------------------------------------

module Deq 
  (
    emptyDeq,        -- Produces an empty Deq      :: Deq a
    isEmptyDeq,      -- Tests if a Deq is empty    :: Deq a -> Bool
    pushDeq,         -- Add element to tail        :: a -> Deq a -> Deq a
    unshiftDeq,      -- Add element to head        :: a -> Deq a -> Deq a
    popDeq,          -- Remove element from tail   :: Deq a -> (a, Deq a)
    shiftDeq,        -- Remove element from head   :: Deq a -> (a, Deq a)
    lastDeq,         -- Lookup last element        :: Deq a -> a
    firstDeq,        -- Lookup first element       :: Deq a -> a
  ) where

-- First implementation variant
-- We can use 'newtype' to declare, since the type takes only one
-- constructor and that constructor has one parameter.
-- In this variant operations take place on both list's sides.
-- Operation on the front are the most efficient. Those on the
-- tail are the costly ones since they require whole list traversal.
newtype Deq a = Deq [a]

-- Second implementation variant
-- Just like the second Queue implementation proposed by the book,
-- this deque variant adds and removes from both lists head.
-- Removal happens on the first list's head until it becomes empty,
-- at which point, second list is reversed and takes the place of
-- first list
data Deq' a = Deq' [a] [a]

emptyDeq :: Deq a
emptyDeq = undefined

isEmptyDeq :: Deq a -> Bool
isEmptyDeq = undefined

pushDeq :: a -> Deq a -> Deq a
pushDeq = undefined

unshiftDeq :: a -> Deq a -> Deq a
unshiftDeq = undefined

popDeq :: Deq a -> (a, Deq a)
popDeq = undefined

shiftDeq :: Deq a -> (a, Deq a)
shiftDeq = undefined

lastDeq :: Deq a -> a
lastDeq = undefined

firstDeq :: Deq a -> a
firstDeq = undefined

