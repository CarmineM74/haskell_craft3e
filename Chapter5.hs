-------------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	Chapter 5
--
-------------------------------------------------------------------------

module Chapter5 where

import Prelude hiding (id)
import Test.QuickCheck
import Data.Char 

-- Data types: tuples and lists
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Introducing tuples, lists and strings
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

type ShopItem = (String,Int)
type Basket   = [ShopItem]

basket1 :: Basket
basket1 = [ ("Salt: 1kg",139) , ("Plain crisps",25) , ("Gin: 1lt",1099) ]

basket2 :: Basket
basket2 = []

basket3 :: Basket
basket3 = [ ("Salt: 1kg",139) , ("Plain crisps",25) , ("Plain crisps",25) ]


-- Tuple types
-- ^^^^^^^^^^^

-- Minimum and maximum of two integers.

minAndMax :: Integer -> Integer -> (Integer,Integer)
minAndMax x y
  | x>=y        = (y,x)
  | otherwise   = (x,y)

-- Adding a pair of intgers.

addPair :: (Integer,Integer) -> Integer
addPair (x,y) = x+y

-- Shifting around the structure of an ((Int,Int),Int).

shift :: ((Integer,Integer),Integer) -> (Integer,(Integer,Integer))
shift ((x,y),z) = (x,(y,z))

-- Selecting parts of a tuple

name  :: ShopItem -> String
price :: ShopItem -> Int

name  (n,p) = n
price (n,p) = p

-- Adding a pair using the built-in selectors, fst and snd.

addPair' :: (Integer,Integer) -> Integer
addPair' p = fst p + snd p

-- Fibonacci numbers: an efficient function, fastFib.

fibStep :: (Integer,Integer) -> (Integer,Integer)
fibStep (u,v) = (v,u+v)

fibPair :: Integer -> (Integer,Integer)
fibPair n
  | n==0        = (0,1)
  | otherwise   = fibStep (fibPair (n-1))

fastFib :: Integer -> Integer
fastFib = fst . fibPair

fibTwoStep :: Integer -> Integer -> (Integer,Integer)
fibTwoStep x y = (y,x+y)

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs m n
  | m /= n = (max m n, 1)
  | otherwise = (max m n, 2)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs m n p
  | m' > p = (m', occurs)
  | m' == p = (m', occurs + 1)
  | otherwise = (p, 1)
  where
    (m',occurs) = maxOccurs m n

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = (x `max` y) `max` z

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z = min (min x y) z

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder x y z
  | (x <= y) && (y <= z) = True
  | otherwise = False

between :: Integer -> Integer -> Integer -> Bool
between x y z
  | weakAscendingOrder x y z = True
  | ((x-y) >= 0) && ((y-z) >= 0) = True
  | otherwise = False

middle :: Integer -> Integer -> Integer -> Integer
middle x y z
  | between x y z = y
  | between x z y = z
  | otherwise = x

orderTriple :: Integer -> Integer -> Integer -> (Integer,Integer,Integer)
orderTriple x y z = (mn, md, mx)
  where
    mx = maxThree x y z
    mn = minThree x y z
    md = middle x y z

-- Introducing algebraic types
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- We give a sequence of examples of increasing complexity ...


-- Product types
-- ^^^^^^^^^^^^^

-- A person is represented by their name and age ...

data People = Person Name Age

-- where Name and Age are the appropriate synonyms.

type Name = String
type Age  = Int

jemima, ronnie :: People
jemima = Person "Electric Aunt Jemima" 77
ronnie = Person "Ronnie" 14

-- Turning a person into a string.

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n

-- An alternative to Age,

data NewAge = Years Int


-- Alternatives
-- ^^^^^^^^^^^^

-- A shape in a simple geometrical program is either a circle or a
-- rectangle. These alternatives are given by the type

data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
	     deriving (Eq,Ord,Show,Read)

shape1 = Circle 3.0
shape2 = Rectangle 45.9 87.6

-- Pattern matching allows us to define functions by cases, as in,

isRound :: Shape -> Bool
isRound (Circle _)      = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False

-- and also lets us use the components of the elements:

area :: Shape -> Float
area (Circle r)      = pi*r*r
area (Rectangle h w) = h*w
area (Triangle a b c) = sqrt (s*(s-a)*(s-b)*(s-c))
  where
    s = 0.5 * (a+b+c)

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle w h) = (2 * w) + (2 * h)
perimeter (Triangle a b c) = a + b + c

isRegular :: Shape -> Bool
isRegular (Circle _) = True
isRegular (Rectangle w h) = w == h
isRegular (Triangle a b c) = (a == b) && (b == c)

type ItemName = String
type Pences = Int
data ShopItem' = Item ItemName Pences

type Point = (Int, Int)
data NewShape = Circle2 Float Point|
             Rectangle2 Float Float Point |
             Triangle2 Float Float Float Point
	     deriving (Eq,Ord,Show,Read)

move :: Int -> Int -> NewShape -> NewShape
move nx ny (Circle2 r (x,y)) = Circle2 r (x+nx,y+ny)
move nx ny (Rectangle2 w h (x,y)) = Rectangle2 w h (x+nx,y+ny)
move nx ny (Triangle2 a b c (x,y)) = Triangle2 a b c (x+nx,y+ny)

minMax :: Float -> Float -> (Float, Float)
minMax a b = (min a b, max a b)

-- 5.13
overlap :: NewShape -> NewShape -> Bool
overlap (Circle2 r (x,y)) (Circle2 r' (x',y')) = d < (min r r')
  where
    a = abs (x - x')
    b = abs (y - y')
    d = sqrt (fromIntegral ((a*a) + (b*b)))

-- se la distanza assoluta tra i centri dei due rettangoli sull'asse x e' minore della somma delle semilarghezze dei rettangoli e quella sull'asse y e' minore delle semialtezze allora i rettangoli sono sovrapposti.
overlap (Rectangle2 w h (x,y)) (Rectangle2 w' h' (x',y')) = (a < semi_x) && (b < semi_y)
  where
    a = fromIntegral (abs (x - x'))
    b = fromIntegral (abs (y - y'))
    semi_x = (w + w') / 2
    semi_y = (h + h') / 2

-- Mancano le basi matematiche per risolvere il problema :(
overlap (Triangle2 a b c (x,y)) (Triangle2 a' b' c' (x',y')) = undefined

-- Derived instances ...

--	data Season = Spring | Summer | Autumn | Winter 
--	              deriving (Eq,Ord,Enum,Show,Read)

-- Lists in Haskell
-- ^^^^^^^^^^^^^^^^

-- Various examples of lists

list1 :: [Integer]
list1 = [1,2,3,4,1,4]

list2 :: [Bool]
list2 = [True]

list3 :: String
list3 = ['a','a','b']

list4 :: String
list4 = "aab"

list5 :: [ Integer -> Integer ]
list5 = [fastFib,fastFib]

list6  :: [ [Integer] ]
list6 = [[12,2],[2,12],[]]

list7 :: [Integer]
list7 = [2 .. 7]

list8 :: [Float]
list8 = [3.1 .. 7.0]

list9 :: String
list9 = ['a' .. 'm']

list10 :: [Integer]
list10 = [7,6 .. 3]

list11 :: [Float]
list11 = [0.0,0.3 .. 1.0]

list12 :: String
list12 = ['a','c' .. 'n']


-- List comprehensions
-- ^^^^^^^^^^^^^^^^^^^
-- Examples of list comprehensions

ex :: [Integer]
ex = [2,4,7]

comp1 :: [Integer]
comp1 = [ 2*n | n<-ex]

comp2 :: [Bool]
comp2 = [ isEven n | n<-ex ]

isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)

comp3 :: [Integer]
comp3 = [ 2*n | n <- ex , isEven n , n>3 ]

-- Add all the pairs in a list of pairs.

addPairs :: [(Integer,Integer)] -> [Integer] 
addPairs pairList = [ m+n | (m,n) <- pairList ]

-- Return only the sums of pairs which are increasing.

addOrdPairs :: [(Integer,Integer)] -> [Integer]
addOrdPairs pairList = [ m+n | (m,n) <- pairList , m<n ]

-- Return only the digits in a String.

digits :: String -> String
digits st = [ ch | ch<-st , isDigit ch ] 

-- Are all the integers in a list even? or odd?

allEven, allOdd :: [Integer] -> Bool
allEven xs = (xs == [x | x<-xs, isEven x])
allOdd xs  = ([] == [x | x<-xs, isEven x])

-- Summing the radii of the circles in a list, ignores the other shapes

totalRadii :: [Shape] -> Float
totalRadii shapes = sum [r | Circle r <- shapes]

-- Extracting all the singletons in a list of integer lists, 
-- ignoring the other lists.

sings :: [[Integer]] -> [Integer]
sings xss = [x | [x] <-xss ]


-- 5.18
doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2*x | x <- xs]

-- 5.19
capitalize :: String -> String
capitalize s = [ toUpper c | c <- s]

capitalizeLetters :: String -> String
capitalizeLetters s = capitalize ([c | c <- s, isLetter c])

-- 5.20
isDivisor :: Integer -> Integer -> Bool
isDivisor n m
  | mod n m == 0 = True
  | otherwise = False

divisors :: Integer -> [Integer]
divisors n = [x | x<-[1..n], isDivisor n x]

isPrime :: Integer -> Bool
isPrime n 
  | divisors n == [1,n] = True
  | otherwise = False

-- 5.21
matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x | x <- xs, x == n]

elem' :: Integer -> [Integer] -> Bool
elem' x xs = length (matches x xs) > 0

-- 5.22
onSeparateLines :: [String] -> String
onSeparateLines ss = concat [ s ++ "\n" | s <- ss] 

-- 5.23
duplicate :: String -> Integer -> String
duplicate s n
  | n == 0 = ""
  | otherwise = concat [s | x <- [1..n]]

-- 5.24
pushRight :: String -> Int -> String
pushRight s linelength
  | linelength <= (length s) = s
  | otherwise = (duplicate " " (toInteger padding)) ++ s
    where
      padding = linelength - (length s)

-- 5.26
fibTable :: Integer -> String
fibTable n = concat (["n\tfib n\n"] ++ [ (show x) ++ "\t" ++ (show (fastFib x)) ++ "\n" | x <- [0..n]])


-- A library database
-- ^^^^^^^^^^^^^^^^^^

-- Types

type Person = String
type Book   = String

type Database = [ (Person , Book) ]

-- An example database.

exampleBase :: Database
exampleBase 
  = [ ("Alice" , "Tintin")  , ("Anna" , "Little Women") ,
      ("Alice" , "Asterix") , ("Rory" , "Tintin") ]

-- The books borrowed by a particular person in the given database.

books       :: Database -> Person -> [Book]
books dBase findPerson
  = [ book | (person,book) <- dBase , person==findPerson ]

-- 5.28

borrowers   :: Database -> Book -> [Person]
borrowers dBase findBook = [person | (person,book) <- dBase, book == findBook] 

borrowed    :: Database -> Book -> Bool
borrowed dBase book = length (borrowers dBase book) > 0

numBorrowed :: Database -> Person -> Int
numBorrowed dBase person = length (books dBase person)

-- Making a loan is done by adding a pair to the database.

makeLoan   :: Database -> Person -> Book -> Database
makeLoan dBase pers bk = [ (pers,bk) ] ++ dBase

-- To return a loan.

returnLoan   :: Database -> Person -> Book -> Database
returnLoan dBase pers bk
  = [ pair | pair <- dBase , pair /= (pers,bk) ]

-- Testing the database.

-- Commented out because borrowed is not defined here.

-- test1 :: Bool
-- test1 = borrowed exampleBase "Asterix"

test2 :: Database
test2 = makeLoan exampleBase "Alice" "Rotten Romans"

-- QuickCheck properties for the database

-- Check that bk is in the list of loaned books to pers
-- after making the loan of book to pers

prop_db1 :: Database -> Person -> Book -> Bool

prop_db1 dBase pers bk =
    elem bk loanedAfterLoan == True
         where
           afterLoan = makeLoan dBase pers bk
           loanedAfterLoan = books afterLoan pers

-- Check that bk is not in the list of loaned books to pers
-- after returning the loan of book to pers

prop_db2 :: Database -> Person -> Book -> Bool

prop_db2 dBase pers bk =
    elem bk loanedAfterReturn == False
         where
           afterReturn = returnLoan dBase pers bk
           loanedAfterReturn = books afterReturn pers


