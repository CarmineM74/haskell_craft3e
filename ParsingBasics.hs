-------------------------------------------------------------------------
-- 
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
--      Case study: Parsing expressions	
-- 
--      Note that this is not a monadic approach to parsing.	
-- 
---------------------------------------------------------------------------                                                     

module ParsingBasics where

import Data.Char

infixr 5 >*>
--  
-- Syntactic types							
--  
type Var = Char
data Expr = Lit Int | Var Var | Op Op Expr Expr
      deriving (Show)
data Op   = Add | Sub | Mul | Div | Mod
      deriving (Show)
--  
-- The type of parsers.						
--  
type Parse a b = [a] -> [(b,[a])]
--  
-- Some basic parsers						
--  
--  
-- Fail on any input.						
--  
none :: Parse a b
none inp = []
--  
-- Succeed, returning the value supplied.				
--  
succeed :: b -> Parse a b 
succeed val inp = [(val,inp)]
--  
-- token t recognises t as the first value in the input.		
--  
token :: Eq a => a -> Parse a a
token t (x:xs) 
  | t==x 	= [(t,xs)]
  | otherwise 	= []
token t []    = []
--  
-- spot whether an element with a particular property is the 	
-- first element of input.						
--  
spot :: (a -> Bool) -> Parse a a
spot p (x:xs) 
  | p x 	= [(x,xs)]
  | otherwise 	= []
spot p []    = []
--  
-- Examples.							
--  
bracket = token '('
dig     =  spot isDigit
--  
-- Combining parsers						
--  
--  
-- alt p1 p2 recognises anything recogniseed by p1 or by p2.	
--  
alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = p1 inp ++ p2 inp
exam1 = (bracket `alt` dig) "234" 
--  
-- Apply one parser then the second to the result(s) of the first.	
--  

(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
-- 	
(>*>) p1 p2 inp 
  = [((y,z),rem2) | (y,rem1) <- p1 inp , (z,rem2)  <- p2 rem1 ]
--  
-- Transform the results of the parses according to the function.	
--  
build :: Parse a b -> (b -> c) -> Parse a c
build p f inp = [ (f x,rem) | (x,rem) <- p inp ]
--  
-- Recognise a list of objects.					
--  
-- 	
list :: Parse a b -> Parse a [b]
list p = (succeed []) `alt`
         ((p >*> list p) `build` convert)
         where
         convert = uncurry (:)

digsToNum :: String -> Int
digsToNum val = read val :: Int

digList :: Parse Char [Char]
digList = neList dig

--  
-- From the exercises...						
--  
neList   :: Parse a b -> Parse a [b] 
neList p = (p >*> list p) `build` convert
  where
    convert = uncurry (:)

optional :: Parse a b -> Parse a [b]
optional p inp =
  case p inp of
    []          -> [([],inp)]
    ((x,rem):_) -> [([x],rem)]

-- 17.11
-- Not quite sure that when n == 0 the result should be []
-- Still I can argue that asking to match 0 objects can always
-- return a successful result because it means no processing at all?
nTimes :: Int -> Parse a b -> Parse a [b]
nTimes 0 _ = succeed []
nTimes n p = (p >*> (nTimes (n-1) p)) `build` (uncurry (:)) 

--  
-- A parser for expressions					
--  
--  
-- The parser has three components, corresponding to the three	
-- clauses in the definition of the syntactic type.		
--  
parser :: Parse Char Expr
parser = (litParse `alt` varParse) `alt` opExpParse
--  
-- Spotting variables.						
--  
varParse :: Parse Char Expr
varParse = spot isVar `build` Var

isVar :: Char -> Bool
isVar x = ('a' <= x && x <= 'z')
--  
-- Parsing (fully bracketed) operator applications.		
--  
opExpParse 
  = (token '(' >*>
     parser    >*>
     spot isOp >*>
     parser    >*>
     token ')') 
     `build` makeExpr

makeExpr (_,(e1,(bop,(e2,_)))) = Op (charToOp bop) e1 e2

operations :: [(Char,Op)]
operations = [('+',Add),('-',Sub),('*',Mul),('/',Div),('%',Mod)]

-- 17.12
isOp :: Char -> Bool
isOp c = length (filter ((==c).fst) operations) > 0

charToOp :: Char -> Op
charToOp c = snd $ head $ filter ((==c).fst) operations

--  
-- A number is a list of digits with an optional ~ at the front. 
--  
litParse 
  = ((optional (token '~')) >*>
     (neList (spot isDigit)))
     `build` (charlistToExpr.join) 
     where
     join = uncurry (++)
--  
-- From the exercises...						
--  
--  17.14
charlistToExpr :: [Char] -> Expr
charlistToExpr l@(x:xs)
  | x == '~' = Lit (read ('-':xs) :: Int)
  | otherwise = Lit (read l :: Int)

-- 17.15

assignParse :: Parse Char (Expr, (Char, Expr))
assignParse = varParse >*> (token ':') >*> opExpParse

-- 17.20
-- "[2,-3,45]" -> [2,-3,45]

intObj :: Parse Char Int
intObj = (neList (spot isDigit)) `build` read 

-- Pattern for resulting element [(n,[])]
-- (('[',([(x,((sep,y):ys))],']')),[])
listIntObj = list (intObj >*> (list ((token ',') >*> intObj)))
makeInt ((n,ns):zs) = n : (map snd ns)
makeInt _ = []

listOfInts 
  = (token '[' >*>
     listIntObj >*>
     token ']') 

-- Last element pattern:
-- (('[',([(x,((sep,y):ys))],']')),[])
-- build p f inp = [ (f x,rem) | (x,rem) <- p inp ]
makeList :: (t, ([(a, [(a1, a)])], t1)) -> [a]
makeList (_,(((n,((_,m):zs)):ys),_)) = n : m : (map snd zs)
makeList _ = []

parseListOfInts inp = concat [ x : y : (map snd ys) | ((_,([(x,((sep,y):ys))],_)),[]) <- xs ]
  where
    xs = listOfInts inp

--  
-- A grammar for unbracketed expressions.				
-- 								
-- eXpr  ::= Int | Var | (eXpr Op eXpr) |				
--           lexpr mop mexpr | mexpr aop eXpr			
-- lexpr ::= Int | Var | (eXpr Op eXpr)				
-- mexpr ::= Int | Var | (eXpr Op eXpr) |	lexpr mop mexpr		
-- mop   ::= 'a' | '/' | '\%'					
-- aop   ::= '+' | '-'						
--  
--  
-- The top-level parser						
--  
topLevel :: Parse a b -> [a] -> b
topLevel p inp
  = case results of
      [] -> error "parse unsuccessful"
      _  -> head results
    where
    results = [ found | (found,[]) <- p inp ]
--  
-- The type of commands.						
--  
data Command = Eval Expr | Assign Var Expr | Null
commandParse :: Parse Char Command
commandParse = commandParse 	 -- dummy definition
--  
-- From the exercises.						
--  
-- tokenList :: [a] -> Parse a [a]
-- spotWhile :: (a -> Bool) -> Parse a [a]
