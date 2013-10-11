------------------------------
-- Ex 16.12                 --
------------------------------

module PrioQueue
 (  PrioQ,
    initQ,            -- :: PrioQ a
    isEmptyQ,         -- :: PrioQ a -> Bool
    addQ,             -- :: a -> Priority -> PrioQ a -> PrioQ a
    remQ,             -- :: PrioQ a -> (a, PrioQ a)
 ) where

import Data.List
import Data.Ord

type Priority = Int
newtype PrioQ a = PrioQ [(Priority, a)]

initQ :: PrioQ a
initQ = PrioQ []

isEmptyQ :: PrioQ a -> Bool
isEmptyQ (PrioQ []) = True
isEmptyQ _ = False

addQ :: a -> Priority -> PrioQ a -> PrioQ a
addQ x prio q@(PrioQ xs)
  | isEmptyQ q = PrioQ [(prio, x)]
  | otherwise = PrioQ (sortBy (comparing fst) ((prio,x):xs))

remQ :: PrioQ a -> (a, PrioQ a)
remQ (PrioQ []) = error "Cannot remove from an empty queue"
remQ (PrioQ xs) = (y, PrioQ rest)
  where
    (rest,h) = splitAt (length xs - 1) xs
    y = snd $ head h

