-- Ex 16.30
module Stree 
  (Stree,
   nil,           -- Stree a
   isNil,         -- Stree a -> Bool  
   isNode,        -- Stree a -> Bool
   leftSub,       -- Stree a -> Stree a 
   rightSub,      -- Stree a -> Stree a 
   treeVal,       -- Stree a -> a
   insTree,       -- Ord a => a -> Stree a -> Stree a 
   delete,        -- Ord a => a -> Stree a -> Stree a
   minTree,       -- Ord a => Stree a -> Maybe a
   elemT,         -- Ord a => a -> Stree a -> Bool
   size           -- Stree a -> Int
  ) where

data Stree a = Nil |
               Node a Int (Stree a) (Stree a)
               deriving (Show)

nil :: Stree a
nil = Nil

isNil :: Stree a -> Bool
isNil Nil = True
isNil _ = False

isNode :: Stree a -> Bool
isNode Nil = False
isNode _ = True

leftSub :: Stree a -> Stree a
leftSub Nil = error "leftSub"
leftSub (Node v s t1 t2) = t1

rightSub :: Stree a -> Stree a
rightSub Nil = error "rightSub"
rightSub (Node v s t1 t2) = t2

treeVal :: Stree a -> a
treeVal Nil = error "treeVal"
treeVal (Node v _ _ _) = v

insTree :: Ord a => a -> Stree a -> Stree a
insTree v Nil = Node v 1 Nil Nil
insTree v t@(Node x s t1 t2)
  | v == x = t 
  | v > x = Node x (s+1) t1 (insTree v t2)
  | v < x = Node x (s+1) (insTree v t1) t2

delete :: Ord a => a -> Stree a -> Stree a
delete v (Node x s t1 t2)
  | v < x = Node x (size nt1 + size t2) nt1 t2
  | v > x = Node x (size t1 + size nt2) t1 nt2
  | isNil t1 = t2
  | isNil t2 = t1
  | otherwise = join t1 t2
  where
    nt1 = delete v t1
    nt2 = delete v t2

size :: Stree a -> Int
size Nil = 0
size (Node _ s _ _) = s

join :: Ord a => Stree a -> Stree a -> Stree a
join t1 t2 = Node min (size t1 + size nt2) t1 nt2
  where
    (Just min) = minTree t2
    nt2 = delete min t2

minTree :: Ord a => Stree a -> Maybe a
minTree Nil = Nothing
minTree (Node v s t1 t2)
 | isNil t1 = Just v 
 | otherwise = minTree t1 

flattenT :: Stree a -> [a]
flattenT Nil = []
flattenT (Node x s t1 t2) = x:(flattenT t1)++(flattenT t2)

elemT :: Ord a => a -> Stree a -> Bool 
elemT v = elem v . flattenT
