module ExChap7 where
import Data.Char
import Prelude hiding (product, and, or, reverse, unzip, minimum, maximum,
                       drop, splitAt, take)

-- 7.1
digits                  :: String -> String
digits [] = []
digits (x:xs) = (if isDigit x then [x] else []) ++ (digits xs)

firstIntegerPlusOne     :: String -> Int
firstIntegerPlusOne st =
  case (digits st) of
    []    -> 0
    (x:_) -> (read [x] :: Int) + 1

-- 7.2
addFirstTwo             :: [Int] -> Int
addFirstTwo [] = 0
addFirstTwo (x:[]) = x
addFirstTwo (x:xs) = x + (head xs)

-- 7.3
addFirstTwo'            :: [Int] -> Int
addFirstTwo' xs = sum $ take 2 xs

-- 7.4
firstDigit              :: String -> Char 
firstDigit [] = '\0'
firstDigit st = head [x | x <- st, isDigit x]

-- 7.5
product                 :: [Int] -> Int
product [] = 1
product (x:xs) = x * (product xs)

-- 7.6
and                     :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (and xs)

or                      :: [Bool] -> Bool
or [] = False
or (x:xs) = x || (or xs)

-- 7.8
elemNum                 :: Integer -> [Integer] -> Integer
elemNum _ [] = 0
elemNum x (y:ys)
  | x == y = 1 + elemNum x ys
  | otherwise = elemNum x ys

elemNum'                :: Integer -> [Integer] -> Integer
elemNum' x ys = sum [1 | y <- ys, x == y]

-- 7.9
unique                  :: [Integer] -> [Integer]
unique [] = []
unique (y:ys)
  | elemNum y ys == 0 = y : unique ys
  | otherwise = unique ys

-- 7.11 primitive recursive definitions of reverse and unzip
reverse                 :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

unzip                   :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip (x:xs) = ([fst x] ++ (fst step),[snd x] ++ (snd step)) 
  where
    step = unzip xs

-- 7.12
dropWhileGt             :: Integer -> [Integer] -> [Integer]
dropWhileGt _ [] = []
dropWhileGt x (y:ys)
  | x <= y = dropWhileGt x ys
  | otherwise = y : dropWhileGt y ys

minimum                 :: [Integer] -> Integer
minimum [] = undefined
minimum (x:xs) 
  | rest == [] = x
  | otherwise = minimum rest 
  where
    rest = dropWhileGt x xs
    

minimum'                 :: [Integer] -> Integer
minimum' [] = undefined
minimum' (x:xs)
  | xs == [] = x
  | x <= (minimum' xs) = x
  | otherwise = minimum' xs

-- 7.14
isSorted                :: [Integer] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:(q:p)) = (x <= q) && isSorted (q:p)

-- 7.19

insLex                  :: (Integer,Integer) -> [(Integer,Integer)] -> [(Integer, Integer)]
insLex (a,b) [] = [(a,b)]
insLex (a,b) ((c,d):ys)
  | a < c = (a,b) : ((c,d):ys)
  | a > c = (c,d) : insLex (a,b) ys
  | (a == c) && (b <= d) = (a,b) : ((c,d):ys)
  | (a == c) && (b > d) = (c,d) : insLex (a,b) ys

iSortLex                :: [(Integer,Integer)] -> [(Integer,Integer)]
iSortLex [] = []
iSortLex (x:xs) = insLex x (iSortLex xs)


-- 7.20

take                    :: Int -> [a] -> [a]
take 0 _  = []
take _ [] = []
take n (x:xs)
  | n > 0         = x : take (n-1) xs
take _ _          = error "ExChap7.take: negative argument"

drop                    :: Int -> [a] -> [a]
drop _ []         = []
drop 0 xs         = xs
drop n (x:xs)
  | n > 0         = drop (n-1) xs
  | otherwise     = error "ExChap7.drop: negative argument"

splitAt                 :: Int -> [a] -> ([a], [a])
splitAt _ []      = ([], [])
splitAt n xs
  | n > 0         = (take n xs, drop n xs)
  | otherwise     = ([], xs)


-- 7.24
qSort                   :: [Integer] -> [Integer]
qSort []          = []
qSort (x:xs)      = qSort [ y | y <- xs, y <= x] ++ [x] ++ qSort [ y | y <- xs, y > x]


qSortDesc               :: [Integer] -> [Integer]
qSortDesc xs      = reverse $ qSort xs

qSortDesc'              :: [Integer] -> [Integer]
qSortDesc' []     = []
qSortDesc' (x:xs) = qSortDesc' [ y | y <- xs, y > x] ++ [x] ++ qSortDesc' [ y | y <- xs, y <= x]

qSortDescNoDups         :: [Integer] -> [Integer]
qSortDescNoDups []     = []
qSortDescNoDups (x:xs) = qSortDescNoDups [ y | y <- xs, y > x] ++ [x] ++ qSortDescNoDups [ y | y <- xs, y < x]

-- 7.25

indicesOf         :: String -> String -> [Integer]
indicesOf [] _    = []
indicesOf _ []    = []
indicesOf s1 s2   = [fromIntegral pos | pos <- [0..(length s2)-1], elem (s2 !! pos) s1]

dropUntilFound    :: Char -> String -> String
dropUntilFound _ []   = []
dropUntilFound '\0' _ = []
dropUntilFound c (x:xs)
  | c == x            = (x:xs)
  | otherwise         = dropUntilFound c xs

-- Sublist
-- A list is a sublist of another if the elements of the first list
-- occur in the second in the same order
-- Ex: "ship" is a sublist of "Fish & Chips" but not of "hippies"
sublist           :: String -> String -> Bool
sublist s1 s2     
  | length s1 <= length s2 = isSorted indices && (length indices >= length s1)
  | otherwise             = False
  where
    indices = indicesOf s1 (dropUntilFound (head s1) s2)

-- A list is a subsequence of another list if the elements occur in the
-- second next to each other
-- Ex: "Chips" is a subsequence of "Fish & Chips" but not of "Chimps"
-- WRONG DESIGN
-- Correct version is in Chapter7.hs
subsequence       :: String -> String -> Bool
subsequence s1 s2 
  | length s1 <= length s2  = (take len found) == s1
  | otherwise              = False
  where
    len   = length s1
    found = dropUntilFound (head s1) s2

data Subkind = Sublist | Subsequence | Neither
                deriving (Show)

-- Guard order is relevant since a subsequence has stricter requirements
-- than a sublist. A subsequence is also a sublist but not vice versa.
sublistOrSubsequence  :: String -> String -> Subkind
sublistOrSubsequence s1 s2
  | subsequence s1 s2 = Subsequence
  | sublist s1 s2     = Sublist
  | otherwise         = Neither
