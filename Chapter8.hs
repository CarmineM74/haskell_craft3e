-------------------------------------------------------------------------
--
--	Haskell: The Craft of Functional Programming, 3e
--	Simon Thompson
--	(c) Addison-Wesley, 1996-2011.
--
--	Chapter 8
--
-------------------------------------------------------------------------

module Chapter8 where

import Data.Time
import System.Locale
import System.IO.Unsafe
import System.IO
import Test.QuickCheck

--
-- Basic types and functions over the type
--

-- A type of moves

data Move = Rock | 
            Paper | 
            Scissors
            deriving Eq

-- Showing Moves in an abbreviated form.

instance Show Move where
      show Rock = "r"
      show Paper = "p"
      show Scissors = "s"

-- For QuickCheck to work over the Move type.

instance Arbitrary Move where
  arbitrary     = elements [Rock, Paper, Scissors]

-- Convert from 0,1,2 to a Move

convertToMove :: Integer -> Move

convertToMove 0 = Rock
convertToMove 1 = Paper
convertToMove 2 = Scissors

-- Convert a character to the corresponding Move element.
  
convertMove :: Char -> Move
    
convertMove 'r' = Rock
convertMove 'R' = Rock
convertMove 'p' = Paper
convertMove 'P' = Paper
convertMove 's' = Scissors
convertMove 'S' = Scissors

-- Outcome of a play
--   +1 for first player wins
--   -1 for second player wins
--    0 for a draw
-- 8.1
outcome :: Move -> Move -> Integer

outcome p1 p2
  | p1 == beat p2 = 1
  | p2 == beat p1 = (-1)
  | otherwise = 0

-- Outcome of a tournament
-- 8.2
tournamentOutcome :: Tournament -> Integer

tournamentOutcome t = sum [outcome p1 p2 | (p1,p2) <- zip (fst t) (snd t) ]

-- Calculating the Move to beat or lose against the 
-- argument Move.

beat, lose :: Move -> Move

beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

-- QuickCheck property about the "sanity" of the 
-- beat and lose functions.

prop_WinLose :: Move -> Bool

prop_WinLose x =
    beat x /= lose x &&
    beat x /= x &&
    lose x /= x


--
-- Strategies
--

type Strategy = [Move] -> Move

-- Random choice of Move

randomStrategy :: Strategy
randomStrategy _ = convertToMove $ randInt 3

-- Constant strategies

sConst :: Move -> Strategy

sConst x _ = x

rock, paper, scissors :: Strategy

rock     = sConst Rock
paper    = sConst Paper
scissors = sConst Scissors

-- Cycle through the three moves.

cycle :: Strategy

cycle moves
  = case (length moves) `rem` 3 of 
      0 -> Rock
      1 -> Paper
      2 -> Scissors

-- Play the move that would have lost the opponent's last play.
-- 8.3
sWonLast  :: Move -> Strategy
sWonLast start moves
  | moves == [] = beat start
  | otherwise = beat (head moves)

sLostLast :: Move -> Strategy
sLostLast start moves
  | moves == [] = lose start
  | otherwise = lose (head moves)

-- 8.4

playToWin :: Move -> Move
playToWin move =
  case outcome first_available_move second_available_move of
    1 -> first_available_move
    -1 -> second_available_move
  where
    available_moves = [m | m <- [Rock,Paper,Scissors], m /= move]
    (first_available_move, second_available_move) = (head available_moves,last available_moves)

sRandButTwo :: Strategy
sRandButTwo moves
  | (length moves >= 2) && (not sametwo) = convertToMove $ randInt 3
  | otherwise = playToWin last_move
  where
    last_move = head moves
    sametwo = last_move == (moves !! 1)

-- 8.5

computeFreqsAcc :: (Int, Int, Int) -> [Move] -> (Int, Int, Int)
computeFreqsAcc (fr,fp,fs) [] = (fr,fp,fs)
computeFreqsAcc (fr,fp,fs) (m:ms) = 
  case m of
    Rock -> computeFreqsAcc (fr+1,fp,fs) ms
    Paper -> computeFreqsAcc (fr,fp+1,fs) ms
    Scissors -> computeFreqsAcc (fr,fp,fs+1) ms

computeFreqs  :: [Move] -> [(Move, Int)]
computeFreqs moves = [(Rock,f_rock),(Paper,f_paper),(Scissors,f_scissors)]
  where
    (f_rock,f_paper,f_scissors) = computeFreqsAcc (0,0,0) moves

leastFreqAcc :: (Move, Int) -> [(Move, Int)] -> (Move, Int)
leastFreqAcc (am, af) [] = (am, af)
leastFreqAcc (am, af) ((m,f):ms)
  | af <= f = leastFreqAcc (am, af) ms
  | otherwise = leastFreqAcc (m, f) ms

leastFreq     :: [(Move, Int)] -> (Move, Int)
leastFreq (m:ms) = leastFreqAcc m (m:ms)

sFreqBased :: Strategy
sFreqBased moves = playToWin least_freq_move
  where
    (least_freq_move, freq) = leastFreq $ computeFreqs moves

-- Echo the previous move; also have to supply starting Move.

echo :: Move -> Strategy

echo start moves 
      = case moves of
          []       -> start
          (last:_) -> last

-- Make a random choice of which Strategy to use, 
-- each turn.

sToss :: Strategy -> Strategy -> Strategy

sToss str1 str2 moves =
    case randInt 2 of
      1 -> str1 moves
      0 -> str2 moves

--
-- Random stuff from time
--

-- Generate a random integer within the IO monad.

randomInt :: Integer -> IO Integer

randomInt n = 
    do
      time <- getCurrentTime
      return ( (`rem` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time)

-- Extract the random number from the IO monad, unsafely!

randInt :: Integer -> Integer

randInt = unsafePerformIO . randomInt 


--- Basics of I/O
--- ^^^^^^^^^^^^^




-- The basics of input/output
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Reading input is done by getLine and getChar: see Prelude for details.

-- 	getLine :: IO String
-- 	getChar :: IO Char

-- Text strings are written using 
-- 	
-- 	putStr :: String -> IO ()
-- 	putStrLn :: String -> IO ()

-- A hello, world program

helloWorld :: IO ()
helloWorld = putStr "Hello, World!"

-- Writing values in general

-- 	print :: Show a => a -> IO ()


-- The do notation: a series of sequencing examples.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Put a string and newline.

-- 	putStrLn :: String -> IO ()
-- 	putStrLn str = do putStr str
-- 	                  putStr "\n"

-- Put four times.

put4times :: String -> IO ()
put4times str 
  = do putStrLn str
       putStrLn str
       putStrLn str
       putStrLn str

-- Put n times

putNtimes :: Integer -> String -> IO ()
putNtimes n str
  = if n <= 1 
       then putStrLn str
       else do putStrLn str
               putNtimes (n-1) str

-- Read two lines, then write a message.

read2lines :: IO ()
read2lines 
  = do getLine
       getLine
       putStrLn "Two lines read."

-- Read then write.

getNput :: IO ()
getNput = do line <- getLine
             putStrLn line

-- Read, process then write.

reverse2lines :: IO ()
reverse2lines
  = do line1 <- getLine
       line2 <- getLine
       putStrLn (reverse line2)
       putStrLn (reverse line1)

-- Last example redefined to use a local definition.

reverse2lines' :: IO ()
reverse2lines'
  = do line1 <- getLine
       line2 <- getLine
       let rev1 = reverse line1
       let rev2 = reverse line2
       putStrLn rev2
       putStrLn rev1

-- Reading an Int.

getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer) 



-- Simple examples

readWrite :: IO ()

readWrite =
    do
      getLine
      putStrLn "one line read"

readEcho :: IO ()

readEcho =
    do
      line <-getLine
      putStrLn ("line read: " ++ line)


-- Adding a sequence of integers

sumInts :: Integer -> IO Integer

sumInts s
  = do n <- getInt
       if n==0 
          then return s
          else sumInts (s+n)

-- Addiing a sequence of integers, courteously.

sumInteract :: IO ()
sumInteract
  = do putStrLn "Enter integers one per line"
       putStrLn "These will be summed until zero is entered"
       sum <- sumInts 0
       putStr "The sum is "
       print sum

-- Copy from input to output

copyEOF :: IO ()

copyEOF = 
    do 
      eof <- isEOF
      if eof  
        then return () 
        else do line <- getLine 
                putStrLn line
                copyEOF

copyInteract :: IO ()

copyInteract = 
    do
      hSetBuffering stdin LineBuffering
      copyEOF
      hSetBuffering stdin NoBuffering

copy :: IO ()

copy =
    do line <- getLine 
       putStrLn line
       copy
      
copyEmpty :: IO ()

copyEmpty =
    do line <- getLine 
       if line == ""
          then return ()
          else do putStrLn line
                  copyEmpty


copyCount :: Integer -> IO ()

copyCount n =
    do line <- getLine 
       if line == ""
          then putStrLn (show n ++ " lines copied.")
          else do putStrLn line
                  copyCount (n+1)

copyN :: Integer -> IO ()

copyN n =
    if n <= 0
    then return ()
    else do line <- getLine
            putStrLn line
            copyN (n-1)

copyWrong :: IO ()

copyWrong =
    do
      line <- getLine
      let whileCopy = 
              do
                if (line == "")
                  then (return ())
                  else 
                    do putStrLn line
                       line <- getLine
                       whileCopy 
      whileCopy


--- Playing Rock - Paper - Scissors
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


--
-- Tournaments
--

-- The Tournament type.

type Tournament = ([Move],[Move])

-- The result of a Tournament, calculates the outcome of each
-- stage and sums the results.

result :: Tournament -> Integer

result = sum . map (uncurry outcome) . uncurry zip


--
-- Play one Strategy against another
--

step :: Strategy -> Strategy -> Tournament -> Tournament

step strategyA strategyB ( movesA, movesB )
     = ( strategyA movesB : movesA , strategyB movesA : movesB )

-- 8.20
playSvsSAcc :: Tournament -> Integer -> Strategy -> Strategy -> Tournament
playSvsSAcc t@(ms,ys) n sa sb
  | n > 0 = playSvsSAcc new_t (n-1) sa sb
  | otherwise = t
  where
    new_t = step sa sb t

playSvsS :: Strategy -> Strategy -> Integer -> Tournament
playSvsS strategyA strategyB n = playSvsSAcc ([],[]) n strategyA strategyB

--
-- Playing interactively
--

-- Top-level function

play :: Strategy -> IO ()

play strategy =
    playInteractive strategy ([],[])

-- The worker function

playInteractive :: Strategy -> Tournament -> IO ()

playInteractive s t@(mine,yours) =
    do 
      ch <- getChar
      if not (ch `elem` "rpsRPS") 
        then showResults t 
        else do let next = s yours 
                putStrLn ("\nI play: " ++ show next ++ " you play: " ++ [ch])
                let yourMove = convertMove ch
                playInteractive s (next:mine, yourMove:yours)


-- Calculate the winner and report the result.

showResults :: Tournament -> IO ()

showResults t = 
    do
      let res = result t
      putStrLn (case compare res 0 of
                  GT ->  "I won!"
                  EQ -> "Draw!"
                  LT -> "You won: well done!")
      
-- Play against a randomly chosen strategy

randomPlay :: IO ()

randomPlay =
    do
      rand <- randomInt 10
      play (case rand of
            0 -> echo Paper
            1 -> sLostLast Scissors
            2 -> const Rock
            3 -> randomStrategy
            4 -> sToss randomStrategy (echo Paper)
            5 -> echo Rock
            6 -> sLostLast Paper
            7 -> sToss (const Rock) (const Scissors)
            8 -> const Paper
            9 -> randomStrategy)
            
