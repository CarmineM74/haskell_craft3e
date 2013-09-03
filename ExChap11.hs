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

-- 11.22
mapFuns' :: [a -> b] -> a -> [b]
mapFuns' fs = flip map fs . flip ($)

-- 11.23

type NevCol = [Float]
type NevMtx = [NevCol]

computeNevilleRow :: NevMtx -> Int -> Int -> (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> (NevMtx, Float, Float)
computeNevilleRow mtx maxCol col f x fac answer error con
  | col <= maxCol = computeNevilleRow mtx' maxCol (col+1) f x fac' answer' error' con
  | otherwise = (mtx,answer,error)
  where
    current_column = mtx !! maxCol -- current column idx == i
    previous_column = mtx !! (maxCol - 1)
    previous_row_value = current_column !! (col - 1) -- A(j-1),i
    previous_step_value = previous_column !! (col - 1) -- A(j-1),(i-1)
    value = ((previous_row_value*fac)-(previous_step_value))/(fac - 1.0)
    fac' = con*con*fac
    current_column' = (take col current_column) ++ [value] ++ (drop (col + 1) current_column)
    mtx' = (take maxCol mtx) ++ [current_column'] ++ (drop (maxCol + 1) mtx)
    errt = max (abs (value - previous_row_value)) (abs (value - previous_step_value))
    (answer',error') = if errt <= error then (value,errt) else (answer,error) 

computeNeville :: NevMtx -> Int -> Int -> (Float -> Float) -> Float -> Float -> Float -> Float -> Float -> Float -> Float
computeNeville mtx maxCol col f x h con safeErr error answer 
  | col <= maxCol = computeNeville mtx'' maxCol col' f x hh con safeErr error' answer' 
  | otherwise = answer
  where
    hh = h / con
    value = (f (x+hh) - f (x-hh)) / (2.0*hh) -- A0,i
    fac = con * con
    column = mtx !! col
    column' = [value] ++ (tail column)
    mtx' = (take col mtx) ++ [column'] ++ (drop (col + 1) mtx)
    (mtx'',answer',error') = computeNevilleRow mtx' col 1 f x fac answer error con
    ith_column = mtx'' !! col
    ith_element = ith_column !! col -- Ai,i
    previous_ith_column = mtx'' !! (col - 1)
    previous_ith_element = previous_ith_column !! (col - 1)
    col' = if (abs (ith_element - previous_ith_element)) >= (safeErr*error') then (maxCol+1) else (col + 1)


drfidr :: (Float -> Float) -> Float -> Float
drfidr f x = computeNeville mtx 10 1 f x h con safeErr error answer
  where
    error = 1.0e50
    safeErr = 2.0
    con = 1.4
    answer = 0.0
    h = 0.16
    p0 = (f (x+h) - f (x-h)) / (2.0*h)
    mtx = [[p0]++(replicate 9 0.0)] ++ replicate 9 (replicate 10 0.0)


slope :: (Float -> Float) -> (Float -> Float)
slope f = (\x -> drfidr f x)
