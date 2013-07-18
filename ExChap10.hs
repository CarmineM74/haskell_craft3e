module ExChap10 where
import Prelude hiding (init, last, unzip)
import Data.List hiding (init, last, unzip)

-- 10.2
alwaysOne :: a -> Integer
alwaysOne _ = 1

length' :: [a] -> Integer
length' xs = sum $ map alwaysOne xs

-- 10.3

greaterOne :: Integer -> Bool
greaterOne n = n>1

addOne :: Integer -> Integer
addOne n = n+1

addUp :: [Integer] -> [Integer]
addUp ns = filter greaterOne (map addOne ns)

gtOrEqOne :: Integer -> Bool
gtOrEqOne n = n >= 1

addUp' :: [Integer] -> [Integer]
addUp' ns = map addOne (filter gtOrEqOne ns)

-- 10.6
squares :: [Integer] -> [Integer]
squares ns = map (^2) ns

sumSquares :: [Integer] -> Integer
sumSquares ns = sum $ squares ns

allGt0 :: [Integer] -> Bool
--allGt0 ns = and $ map (>0) ns
--allGt0 = and . map (>0)
allGt0 = all (>0)

-- 10.7
minimum' :: Ord b => (Integer -> b) -> Integer -> b
minimum' f n = head $ sort values
  where
    values = map f [0..n]

allEqual :: (Integer -> Integer) -> Integer -> Bool
allEqual f n = hs == values
  where
    values = map f [0..n]
    h = head values
    len_values = length values
    hs = replicate len_values h

allEqual' :: (Integer -> Integer) -> Integer -> Bool
allEqual' f n = and $ zipWith (==) values hs
  where
    values = map f [0..n]
    hs = replicate (length values) (head values)

allGt0'' :: (Integer -> Integer) -> Integer -> Bool
allGt0'' f n = and $ map ((>0) . f) [0..n]

isSorted :: [Integer] -> Bool
isSorted ns = (sort ns) == ns

isAscendingOrder :: (Integer -> Integer) -> Integer -> Bool
isAscendingOrder f n = isSorted values
  where
    values = map f [0..n]

-- 10.9
iter :: Integer -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- 10.10
double :: Integer -> Integer
double n = 2 * n

powerTwo :: Integer -> Integer
powerTwo n = iter n double 1

-- 10.13
sumOfSquares :: [Integer] -> Integer
sumOfSquares ns = foldr (+) 0 $ map (^2) ns

-- 10.14
sumOfSquaresPositives :: [Integer] -> Integer
sumOfSquaresPositives = sumOfSquares . filter (>0) 

-- 10.15
last :: [a] -> a
--last xs = head $ foldr f [] xs
--  where
--    f x [] = [x]
--    f _ a = a
last xs = head $ fst $ foldr f ([],len) xs
  where
    len = length xs
    f x (a,l) = if l == len then (x:a,l-1) else (a,l-1)

init :: [a] -> [a]
init xs = fst $ foldr f ([],len) xs
  where
    len = length xs
    f x (a,l) = if l == len then (a,l-1) else (x:a,l-1)

unzip :: [(a,b)] -> ([a],[b])
unzip xs = foldr f ([],[]) xs
  where
    f (x,y) (as,bs) = (x:as,y:bs)

-- 10.18
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p xs = reverse $ fst $ foldr f ([],False) $ reverse xs
  where
    f x (ys,True) = (x:ys,True)
    f x (ys,False)
      | p x = (x:ys,False)
      | otherwise = (ys,True)

--returnLoan   :: Database -> Person -> Book -> Database
--returnLoan dBase pers bk = filterFirst isNotLoaned dBase
--  where
--    isNotLoaned pair = pair /= (pers,bk)

-- 10.19
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = filterFirst p (reverse xs)

