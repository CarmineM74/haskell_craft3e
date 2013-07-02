module ExChap7 where
import Data.Char
import Prelude hiding (product, and, or, reverse, unzip)

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
