-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 12
--
-----------------------------------------------------------------------

-- For Rock-Paper-Scissors examples see RPS.hs

module Chapter12 where

import Pictures hiding (flipH,rotate,flipV,beside,invertColour,
			superimpose,printPicture)
import Data.List
import Data.Char

-- Revisiting the Pictures example, yet again.

flipV :: Picture -> Picture
flipV      = map reverse

beside :: Picture -> Picture -> Picture
beside = zipWith (++)


-- Revisiting the Picture example
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Some of the functions are already (re)defined in this script.
-- Among the other functions mentioned were 

invertColour :: Picture -> Picture
invertColour = map (map invert)

superimpose  :: Picture -> Picture -> Picture
superimpose = zipWith (zipWith combineChar)

-- The definition of combineChar is left as an exercise: it's a dummy definition
-- here.

combineChar :: Char -> Char -> Char
combineChar = combineChar

-- Printing a picture: uses putStr after a newline has been added at the end of
-- every line and the lines are joined into a single string.

printPicture :: Picture -> IO ()
printPicture = putStr . concat . map (++"\n")

-- Ex 12.1
alternatePic :: Picture -> Picture -> Int -> Picture
alternatePic p1 p2 n
  | even n = p1
  | otherwise = p2

-- chessBoard could be simplified by finding a general high order function
-- to pass to foldr which accepts a (Picture -> Picture) function as
-- a parameter (above or beside are such functions)
chessBoard :: Int -> Picture
chessBoard n = foldr (\x a -> above a (alternatePic (invertColour line) line x)) line [1..n-1]
  where
    line = foldr (\x a -> beside a (alternatePic white black x)) black [1..n-1]

-- Ex 12.3
drawDot :: Int -> Int -> Picture -> Picture
drawDot x y p = (take (y-1) p) ++ [row'] ++ (drop y p)
  where
    row = p !! y
    (lefts,rights) = splitAt x row
    row' = (init lefts) ++ ['#'] ++ rights

makePicture :: Int -> Int -> [(Int,Int)] -> Picture
makePicture w h pts = foldr (\(x,y) p -> drawDot x y p) empty pts
  where
    empty = replicate h (replicate w '.')

-- 12.4
pictureToRep :: Picture -> (Int,Int,[(Int,Int)])
pictureToRep p = (w,h,foldr (\r a -> (rowIdxs r (p!!r))++a) [] [0..(h-1)])
  where
    (w,h) = (length (p!!0), length p)
    rowIdxs rownum row = map (\idx -> (idx+1,rownum+1)) $ findIndices (=='#') row

-- Regular expressions

type RegExp = String -> Bool

char :: Char -> RegExp

epsilon = (=="")

char ch = (==[ch])

(|||) :: RegExp -> RegExp ->  RegExp

e1 ||| e2 = 
    \x -> e1 x || e2 x

(<*>) :: RegExp -> RegExp ->  RegExp

e1 <*> e2 =
    \x -> or [ e1 y && e2 z | (y,z) <- splits x ]

(<**>) :: RegExp -> RegExp ->  RegExp

e1 <**> e2 =
    \x -> or [ e1 y && e2 z | (y,z) <- fsplits x ]

splits xs = [splitAt n xs | n<-[0..len]]
    where
      len = length xs

star :: RegExp -> RegExp

star p = epsilon ||| (p <**> star p)
--           epsilon ||| (p <*> star p)
-- is OK as long as p can't have epsilon match

fsplits xs = tail (splits xs)

-- 12.17
range :: Char -> Char -> RegExp
range start stop =
  -- \x -> and $ map ((flip elem) (map chr [ord start .. ord stop])) x
  \x -> elem x [[chr c] | c <- [ord start .. ord stop]]

numbers :: RegExp
numbers = (range '1' '9')<**>(star (range '0' '9'))

fractionals :: RegExp
fractionals = numbers<**>(char '.')<**>(star (char '0'))<**>numbers<**>(range '1' '9')

-- 12.18
countBetween :: Int -> Int -> RegExp -> RegExp
countBetween min max re =
  \x -> elem (length [1 | c <- x, re [c]]) [min .. max] 

maxLength :: Int -> RegExp -> RegExp
maxLength len re =
  \x -> re x && (length x <= len)

--
-- Case studies: functions as data
--

-- Natural numbers as functions.

type Natural a = (a -> a) -> (a -> a)

zero, one, two :: Natural a

zero f = id 
one f  = f 
two f  = f.f

int :: Natural Int -> Int 

int n = n (+1) 0

-- sends representation of n to rep. of n+1

succ :: Natural a -> Natural a
succ = error "succ"

-- sends reps. of n and m to rep. of n+m

plus :: Natural a -> Natural a -> Natural a
plus = error "plus"

-- sends reps. of n and m to rep. of n*m
times :: Natural a -> Natural a -> Natural a
times = error "times"

-- Creating an index
-- ^^^^^^^^^^^^^^^^^

-- See Index.hs

-- Development in practice
-- ^^^^^^^^^^^^^^^^^^^^^^^
-- Defining the .. notation (not executable code).
-- 
-- [m .. n]
--   | m>n         = []
--   | otherwise   = m : [m+1 .. n]

-- [1 .. n] 
--   | 1>n         = []
--   | otherwise   = [1 .. n-1] ++ [n]

-- A simple palindrome check.

simplePalCheck :: String -> Bool
simplePalCheck st = (reverse st == st)

-- The full check

palCheck = simplePalCheck . clean

-- where the clean function combines mapping (capitals to smalls) and
-- filtering (removing punctuation)

clean :: String -> String 

clean = map toSmall . filter notPunct

toSmall  = toSmall	-- dummy definition
notPunct = notPunct	-- dummy definition

-- Auxiliary functions

-- When is one string a subsequence of another? 

subseq :: String -> String -> Bool

subseq []    _  = True
subseq (_:_) [] = False
subseq (x:xs) (y:ys)
  = subseq (x:xs) ys || frontseq (x:xs) (y:ys)

-- When is one strong a subsequece of another, starting at the front?

frontseq :: String -> String -> Bool
frontseq []     _  = True
frontseq (_:_)  [] = False
frontseq (x:xs) (y:ys)
  = (x==y) && frontseq xs ys


-- Understanding programs
-- ^^^^^^^^^^^^^^^^^^^^^^

mapWhile :: (a -> b) -> (a -> Bool) -> [a] -> [b]

mapWhile f p []    = [] 
mapWhile f p (x:xs)
  | p x            = f x : mapWhile f p xs
  | otherwise      = [] 

example1 = mapWhile (2+) (>7) [8,12,7,13,16]


