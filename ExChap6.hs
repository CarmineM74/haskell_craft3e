module ExChap6 where
import Prelude
import Pictures

-- 6.1
-- 6.2
-- 6.3
-- 6.4
superimposeChar       :: Char -> Char -> Char
superimposeChar c c' = if (c == '.') && (c' == '.') then '.' else '#'

-- 6.5
superimposeLine       :: [Char] -> [Char] -> [Char]
superimposeLine l1 l2 = [superimposeChar c1 c2 | (c1,c2) <- (zip l1 l2)]

-- 6.6
superimpose           :: Picture -> Picture -> Picture
superimpose p1 p2 = [superimposeLine l1 l2 | (l1,l2) <- (zip p1 p2)]

-- 6.7
printPicture          :: Picture -> IO ()
printPicture p = putStr (concat [l ++ "\n"| l <- p])

-- 6.8
--width                 :: Picture -> Int
--width p 
--  | p == [] = 0 
--  | otherwise = length (p !! 0)

pickColumn            :: Picture -> Int ->[Char] 
pickColumn p col = [l !! col | l <- p]

rotate90              :: Picture -> Picture
rotate90 p = [ pickColumn flipped i | i <- [0..(width flipped)-1]]
  where
    flipped = flipH p

-- 6.9
rotate90anti          :: Picture -> Picture
rotate90anti p = flipV (flipH (rotate90 p))

-- 6.10
scaleLine             :: [Char] -> Int -> [Char]
scaleLine l factor = concat [replicate factor c | c <- l]

scale                 :: Picture -> Int -> Picture
scale p factor = [ scaleLine (p !! rownum) factor | rownum <- rownums]
  where
    height_p = (height p) - 1
    rownums = concat [replicate factor x | x <- [0..height_p]]

-- 6.17

pad                   :: Picture -> Int -> Picture
pad p p_width = [l ++ (replicate padding '.') | l <- p]
  where
    padding = p_width - (width p)

above'                :: Picture -> Picture -> Picture
above' p1 p2 = p1_padded ++ p2_padded
  where
    max_width = max (width p1) (width p2)
    p1_padded = pad p1 max_width
    p2_padded = pad p2 max_width


-- 6.23
type RlePicture = [[(Int,Char)]]

howManyConsecutive    :: Char -> [Char] -> Int
howManyConsecutive c xs
  | xs == [] = 0
  | c == (head xs) = 1 + (howManyConsecutive c (tail xs))
  | otherwise = 0 

rlePackRow            :: [Char] -> [(Int,Char)]
rlePackRow xs
  | xs == [] = []
  | otherwise = [(consecutives,h)] ++ (rlePackRow rest)
    where
      (h,t) = (head xs, tail xs)
      consecutives = (howManyConsecutive h t) + 1
      rest = drop consecutives xs 

rlePack               :: Picture -> RlePicture
rlePack p = [rlePackRow l | l <- p]

rleUnpackRow          :: [(Int,Char)] -> [Char]
rleUnpackRow xs = concat [ replicate cnt pixel | (cnt,pixel) <- xs]

rleUnpack             :: RlePicture -> Picture
rleUnpack p = [ rleUnpackRow l | l <- p]
