module ExChap6 where
import Prelude hiding (lookup)
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


-- 6.29
type Position = (Int,Int)
type Image = (Picture,Position)

makeImage             :: Picture -> Position -> Image
makeImage pic pos = (pic,pos)

-- 6.30
changePosition        :: Image -> Position -> Image
changePosition img newpos = (fst img, newpos)

-- 6.31
moveImage             :: Image -> Int -> Int -> Image
moveImage img dx dy = (pic,new_pos)
  where
    pic = fst img
    (cur_x,cur_y) = snd img
    new_pos= (cur_x + dx, cur_y + dy)

-- Supermarket billing Extended Exercise

type Name     = String
type Price    = Int
type BarCode  = Int
type Database = [ (BarCode,Name,Price) ]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

type TillType = [BarCode]
type BillType = [(Name,Price)]

lineLength          :: Int
lineLength = 30

--makeBill            :: TillType -> BillType
--formatBill          :: BillType -> String
--produceBill         :: TillType -> String

-- 6.39
formatPence         :: Price -> String
formatPence price = pounds ++ "." ++ padded_pence
  where
    pounds = show (div price 100)
    pence = show (mod price 100)
    padded_pence = (replicate (2 - (length pence)) '0') ++ pence

-- 6.40
computePadding        :: String -> String -> String
computePadding prefix suffix = replicate (lineLength - (length prefix) - (length suffix)) '.'

formatLine          :: (Name,Price) -> String
formatLine (name,price) = name ++ (computePadding name formatted_price) ++ formatted_price ++ "\n"
  where
    formatted_price = formatPence price

-- 6.41
formatLines         :: [ (Name,Price) ] -> String
formatLines ls = concat [formatLine l | l <- ls]

-- 6.42
makeTotal           :: BillType -> Price
makeTotal bill = sum [price | (_,price) <- bill]

-- 6.43
formatTotal         :: Price -> String
formatTotal total = "\n" ++ prefix ++ (computePadding prefix formatted_total)  ++ formatted_total
  where
    prefix = "Total"
    formatted_total = formatPence total

-- 6.44
centerText          :: String -> Int -> Char -> String
centerText text width padchar = padleft ++ text ++ padright
  where
    text_length = length text
    padlr = div (width - text_length) 2
    extra = if ((padlr * 2) + text_length) < width then 1 else 0
    padleft = replicate (padlr+extra) padchar
    padright = replicate padlr padchar


formatBill          :: BillType -> String
formatBill bill = bill_header ++ "\n\n" ++ bill_body ++ bill_total ++ "\n"
  where
    bill_header = centerText "Haskell Store" lineLength ' '
    bill_total = formatTotal $ makeTotal bill
    bill_body = formatLines bill
    
-- 6.45
look                :: Database -> BarCode -> (Name,Price)
look db barcode
  | found == [] = ("Unknown Item",0)
  | otherwise = head found
  where
    found = [(n,p) | (c,n,p) <- db, c == barcode]

-- 6.46
lookup              :: BarCode -> (Name,Price)
lookup barcode = look codeIndex barcode

-- 6.47
makeBill            :: TillType -> BillType
makeBill tills = [lookup t | t <- tills] 

-- 6.48
makeDiscount        :: BillType -> Price
makeDiscount bill = (div (length bottles) 2) * 100
  where
    bottles = [(n,p) | (n,p) <- bill, n == "Dry Sherry, 1lt" ]

formatDiscount      :: Price -> String
formatDiscount discount = formatLine ("Discount", discount) 


formatBill'         :: BillType -> String
formatBill' bill = bill_header ++ "\n\n" ++ bill_body ++ total_discount ++ bill_total ++ "\n"
  where
    bill_header = centerText "Haskell Store" lineLength ' '
    bill_body = formatLines bill
    discount = makeDiscount bill
    total_discount = if discount > 0 then "\n" ++ (formatDiscount discount) else ""
    total = makeTotal bill
    bill_total = formatTotal $ (total - discount) 
