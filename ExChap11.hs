module ExChap11 where
import ExChap6

-- 11.3
composeList :: [a -> a] -> (a -> a)
composeList fs = foldr1 (.) fs

-- 11.7
whitespaces :: String
whitespaces = " \t\n"

myfun :: Char -> Bool
myfun = \ch -> (not . elem ch) whitespaces

-- 11.8
comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y))

total :: (Integer -> Integer) -> (Integer -> Integer)
total f = \n -> (sum . map f) [0..n]

-- 11.9
swapArgs :: (a -> b -> c) -> (b -> a -> c)
swapArgs f = \b a -> f a b

-- 11.11
-- Can't understand how to do it

-- 11.13
mapFuns :: [a -> b] -> a -> [b]
mapFuns fs x = map ($ x) fs

-- 11.17
curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3 f x y z = f (x,y,z)

uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f (x,y,z) = f x y z

curry3' :: ((a,b,c) -> d) -> (a -> b -> c ->d)
curry3' f x y z = curry (\(j,k) -> f (j,k,z)) x y 

curry3'' :: ((a,b,c) -> d) -> (a -> b -> c ->d)
curry3'' f = \x y z -> curry (\(j,k) -> f (j,k,z)) x y

iter :: Integer -> (a -> a) -> (a -> a)
iter n f 
  | n > 0 = f . iter (n-1) f
  | otherwise = id

-- 11.21
iter' :: Int -> (a -> a)  -> (a -> a)
iter' n = foldr (.) id . replicate n 

uncurry3' :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3' f (x,y,z) = uncurry (\j k -> f j k z) (x,y)


