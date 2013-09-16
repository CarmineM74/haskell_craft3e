-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 14, part 1
--      Also covers the properties in Section 14.7
--
-----------------------------------------------------------------------

module Chapter14_1 where

import Prelude hiding (Either(..),either,Maybe(..),maybe)
import Test.QuickCheck
import Control.Monad
import Data.List
import Data.Either

-- Algebraic types
-- ^^^^^^^^^^^^^^^

-- Introducing algebraic types
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- We give a sequence of examples of increasing complexity ...

-- Enumerated types
-- ^^^^^^^^^^^^^^^^
-- Two enumerated types

data Temp   = Cold | Hot
              deriving (Show)

data Season = Spring | Summer | Autumn | Winter
              deriving (Show,Eq,Enum)

-- A function over Season, defined using pattern matching.

weather :: Season -> Temp

weather Summer = Hot
weather _      = Cold

-- The Ordering type, as used in the class Ord.

-- 	data Ordering = LT | EQ | GT

-- Declaring Temp an instance of Eq.

instance Eq Temp where
  Cold == Cold  = True
  Hot  == Hot   = True
  _    == _     = False



-- Recursive algebraic types
-- ^^^^^^^^^^^^^^^^^^^^^^^^^

-- Expressions
-- ^^^^^^^^^^^

-- Representing an integer expression.

data Expr = Lit Integer |
            Add Expr Expr |
            Sub Expr Expr |
-- 14.5
            Mul Expr Expr |
            Div Expr Expr 
                deriving (Show,Eq)

-- Three examples from Expr.

expr1 = Lit 2
expr2 = Add (Lit 2) (Lit 3)
expr3 = Add (Sub (Lit 3) (Lit 1)) (Lit 3)  

-- Evaluating an expression.

eval :: Expr -> Integer

eval (Lit n)     = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
-- 14.5
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2)
  | eval_e2 == 0 = error "Division by zero!"
  | otherwise = eval e1 `div` eval_e2
  where
    eval_e2 = eval e2

-- 14.4
sizeE :: Expr -> Integer
sizeE (Lit _) = 0
sizeE (Add e1 e2) = 1 + (sizeE e1) + (sizeE e2)
sizeE (Sub e1 e2) = 1 + (sizeE e1) + (sizeE e2)

-- Showing an expression.

-- 	instance Show Expr where
-- 
-- 	  show (Lit n) = show n
-- 	  show (Add e1 e2) 
-- 	    = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
-- 	  show (Sub e1 e2) 
-- 	    = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"


-- Trees of integers
-- ^^^^^^^^^^^^^^^^^

-- The type definition.

data NTree = NilT |
             Node Integer NTree NTree
                   deriving (Show,Eq,Read,Ord)
-- Example trees

treeEx1 = Node 10 NilT NilT
treeEx2 = Node 17 (Node 14 NilT NilT) (Node 20 NilT NilT)

-- Definitions of many functions are primitive recursive. For instance,

sumTree,depth :: NTree -> Integer

sumTree NilT            = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth NilT             = 0
depth (Node n t1 t2)  = 1 + max (depth t1) (depth t2)

-- How many times does an integer occur in a tree?

occurs :: NTree -> Integer -> Integer

occurs NilT p = 0
occurs (Node n t1 t2) p
  | n==p        = 1 + occurs t1 p + occurs t2 p
  | otherwise   =     occurs t1 p + occurs t2 p


-- 14.9
leftTree :: NTree -> NTree
leftTree NilT = NilT
leftTree (Node n t1 t2) = Node n (leftTree t1) NilT

rightTree :: NTree -> NTree
rightTree NilT = NilT
rightTree (Node n t1 t2) = Node n NilT (rightTree t2)

-- 14.10
isElemTree :: Integer -> NTree -> Bool
isElemTree _ NilT = False
isElemTree x (Node n t1 t2)
  | x == n = True
  | otherwise = isElemTree x t1 || isElemTree x t2

treeToList :: NTree -> [Integer]
treeToList NilT = []
treeToList (Node n t1 t2) = n:(treeToList t1)++(treeToList t2)

isElemTree' :: Integer -> NTree -> Bool
isElemTree' x t = elem x t_list
  where
    t_list = treeToList t

-- 14.11
minElemTree :: NTree -> Integer
minElemTree NilT = error "Tree is empty!"
minElemTree t = (head . sort) $ treeToList t

maxElemTree :: NTree -> Integer
maxElemTree NilT = error "Tree is empty!"
maxElemTree t = (head . reverse . sort) $ treeToList t

-- 14.12
reflect :: NTree -> NTree
reflect NilT = NilT
reflect (Node n t1 t2) = Node n (reflect t2) (reflect t1) 

-- 14.13
-- Commented out because same definitions occurs
-- in Chapter14_2.
--collapse, sortTree :: NTree -> [Integer]
--collapse NilT = []
--collapse (Node n t1 t2) = (collapse t1)++[n]++(collapse t2)

--sortTree = sort . collapse

-- Rearranging expressions
-- ^^^^^^^^^^^^^^^^^^^^^^^

-- Right-associating additions in expressions.

assoc :: Expr -> Expr

assoc (Add (Add e1 e2) e3)
  = assoc (Add e1 (Add e2 e3)) 
assoc (Add e1 e2) 
  = Add (assoc e1) (assoc e2) 
assoc (Sub e1 e2) 
  = Sub (assoc e1) (assoc e2)
assoc (Lit n) 
  = Lit n
 

-- Infix constructors
-- ^^^^^^^^^^^^^^^^^^

-- An alternative definition of Expr.

data Expr' = Lit' Integer |
             Expr' :+: Expr' |
             Expr' :-: Expr'



-- Mutual Recursion
-- ^^^^^^^^^^^^^^^^

-- Mutually recursive types ...

data Person = Adult Name Address Biog |
              Child Name
data Biog   = Parent String [Person] |
              NonParent String

type Name = String
type Address = [String]

-- ... and functions.

showPerson (Adult nm ad bio) 
  = show nm ++ show ad ++ showBiog bio
showBiog (Parent st perList)
  = st ++ concat (map showPerson perList)

-- Alternative definition of Expr (as used later in the calculator case
-- study.

-- data Expr = Lit Int |
--             Op Ops Expr Expr

-- data Ops  = Add | Sub | Mul | Div 

-- It is possible to extend the type Expr so that it contains
-- conditional expressions, \texttt{If b e1 e2}.

-- data Expr = Lit Int |
--             Op Ops Expr Expr |
--             If BExp Expr Expr

-- Boolean expressions.

data BExp = BoolLit Bool |
            And BExp BExp |
            Not BExp |
            Equal Expr Expr |
            Greater Expr Expr

-- QuickCheck for algebraic types

instance Arbitrary NTree where
  arbitrary = sized arbNTree

arbNTree :: Int -> Gen NTree

arbNTree 0 = return NilT
arbNTree n
    | n>0
        = frequency[(1, return NilT),
                    (3, liftM3 Node arbitrary bush bush)]
          where
            bush = arbNTree (div n 2)

instance Arbitrary Expr where
  arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr

arbExpr 0 = liftM Lit arbitrary
arbExpr n
    | n>0
        = frequency[(1, liftM Lit arbitrary),
                    (2, liftM2 Add bush bush),
                    (2, liftM2 Sub bush bush)]
          where
            bush = arbExpr (div n 2)

prop_assoc :: Expr -> Bool

prop_assoc expr = 
    eval expr == eval (assoc expr)

prop_depth :: NTree -> Bool

prop_depth t =
    size t < 2^(depth t)

size :: NTree -> Integer

size NilT             = 0
size (Node n t1 t2)  = 1 + (size t1) + (depth t2)
