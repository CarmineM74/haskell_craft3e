module ExChap6 where
import Prelude
import Chapter6

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
width                 :: Picture -> Int
width p 
  | p == [] = 0 
  | otherwise = length (p !! 0)

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
