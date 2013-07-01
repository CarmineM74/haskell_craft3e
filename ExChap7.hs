module ExChap7 where
import Data.Char
import Prelude hiding (product, and, or)

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
