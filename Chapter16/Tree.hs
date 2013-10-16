-------------------------------------------------------------------------
--  
--         Tree.hs
--  
-- 	   Search trees as an ADT					
-- 									
--         (c) Addison-Wesley, 1996-2011.					
--  
-------------------------------------------------------------------------
                                                            
module Tree 
  (Tree,
   nil,           -- Tree a
   isNil,         -- Tree a -> Bool  
   isNode,        -- Tree a -> Bool
   leftSub,       -- Tree a -> Tree a 
   rightSub,      -- Tree a -> Tree a 
   treeVal,       -- Tree a -> a
   insTree,       -- Ord a => a -> Tree a -> Tree a 
   delete,        -- Ord a => a -> Tree a -> Tree a
   minTree,        -- Ord a => Tree a -> Maybe a
   elemT,          -- Ord a => a -> Tree a -> Bool
   successor,      -- Ord a => a -> Tree a -> Maybe a
   closest         -- Int -> Tree Int -> Int
  ) where

import Data.List (sort, sortBy)
import Data.Ord (comparing)

data Tree a = Nil | Node a (Tree a) (Tree a)					
--  

nil :: Tree a

nil = Nil

isNil :: Tree a -> Bool
isNil Nil = True
isNil _   = False

isNode :: Tree a -> Bool
isNode Nil = False 
isNode _   = True

leftSub, rightSub :: Tree a -> Tree a

leftSub Nil            = error "leftSub"
leftSub (Node _ t1 _) = t1

rightSub Nil            = error "rightSub"
rightSub (Node v t1 t2) = t2

treeVal  :: Tree a -> a

treeVal Nil            = error "treeVal"
treeVal (Node v _ _) = v

insTree :: Ord a => a -> Tree a -> Tree a

insTree val Nil = (Node val Nil Nil)

insTree val (Node v t1 t2)
  | v==val 	= Node v t1 t2
  | val > v 	= Node v t1 (insTree val t2)	
  | val < v 	= Node v (insTree val t1) t2	

delete :: Ord a => a -> Tree a -> Tree a

delete val (Node v t1 t2)
  | val < v 	= Node v (delete val t1) t2
  | val > v 	= Node v t1 (delete val t2)
  | isNil t2 	= t1
  | isNil t1 	= t2
  | otherwise 	= join t1 t2


minTree :: Ord a => Tree a -> Maybe a

minTree t
  | isNil t 	= Nothing
  | isNil t1 	= Just v
  | otherwise 	= minTree t1
      where
      t1 = leftSub t
      v  = treeVal t

elemT :: Ord a => a -> Tree a -> Bool

elemT x Nil       = False
elemT x (Node y t1 t2)
  | x<y          = elemT x t1
  | x==y         = True
  | otherwise    = elemT x t2



-- The join function is an auxiliary, used in delete, where note that it
-- joins two trees with the property that all elements in the left are
-- smaller than all in the right; that will be the case for the call in
-- delete. 

-- join is not exported.

join :: Ord a => Tree a -> Tree a -> Tree a

join t1 t2 
  = Node mini t1 newt
    where
    (Just mini) = minTree t2
    newt        = delete mini t2

-- Ex 16.29
successor :: Ord a => a -> Tree a -> Maybe a
successor _ Nil = Nothing

successor v (Node x t1 t2)
  | v >= x = successor v t2
  | v < x = if next_successor == Nothing then Just x else next_successor
  where
    next_successor = successor v t1

flattenT :: Tree a -> [a]
flattenT Nil = []
flattenT (Node x t1 t2) = x:(flattenT t1)++(flattenT t2)

-- We can ssume closest will always be called on non empty trees
closest :: Int -> Tree Int -> Int
closest v = snd . head . sortBy (comparing fst) . map (\x -> (abs (v-x),x)) . sort . flattenT
