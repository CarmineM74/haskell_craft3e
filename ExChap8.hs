module ExChap8 where
import Data.Char

-- 8.10

isPalindrome :: String -> Bool
isPalindrome s = s == (reverse s)

testPalindrome :: IO ()
testPalindrome = do
                  putStr "Enter a word: "
                  line <- getLine
                  let test = isPalindrome line
                  putStr "The entered word is "
                  if test == True then
                    putStrLn "a palindrome."
                  else
                    putStrLn "not a palindrome"
                  return ()

-- 8.11
sumTwo  :: IO ()
sumTwo = do
          putStr "Enter first integer: "
          sa <- getLine
          putStr "Enter second integer: "
          sb <- getLine
          let (a,b) = (read sa :: Int, read sb :: Int)
          putStrLn ("The sum is " ++ (show $ a + b))
          return ()

-- 8.12
putNtimes :: Integer -> String -> IO ()
putNtimes n s
  | n == 1 = do 
              putStrLn s
  | otherwise = do
                  putStrLn s
                  putNtimes (n-1) s

putNtimes' :: Integer -> String -> IO ()
putNtimes' n s = putStrLn $ concat [s ++ "\n" | x <- [1..n]]

putNtimes'' :: Integer -> String -> IO ()
putNtimes'' n s = if n <= 1 then
                    putStrLn s
                  else
                    do
                      putStrLn s
                      putNtimes'' (n-1) s

-- 8.13
getInt :: IO Integer
getInt = do line <- getLine
            return (read line :: Integer)

getNInt :: Integer -> IO [Integer]
getNInt n
  | n <= 1 = do
              x <- getInt
              return [x]
  | otherwise = do
                  x <- getInt
                  xs <- getNInt (n-1)
                  return (x : xs)

sumNvalues :: IO () 
sumNvalues = do
              putStr "How many values? "
              n <- getInt
              vs <- getNInt n
              let s = sum vs
              putStrLn ("The sum is " ++ (show s))

-- 8.14
getLinesAcc :: [String] -> IO [String]
getLinesAcc lines = do
                      line <- getLine
                      if line == "" then
                        return lines
                      else
                        getLinesAcc (line:lines)

copyToOutput :: IO [String]
copyToOutput = do
                lines <- getLinesAcc []
                return lines

analyseLines :: [String] -> (Int,Int,Int)
analyseLines [] = (0,0,0)
analyseLines lines = (nr_lines,nr_words,nr_chars)
  where
    nr_lines = length lines
    nr_words = sum [ length $ words l | l <- lines ]
    nr_chars = length (concat lines)


dumpStats :: (Int,Int,Int) -> IO ()
dumpStats (nr_lines,nr_words,nr_chars) =  do
                                            putStrLn ("Total nr of lines: " ++ (show nr_lines))
                                            putStrLn ("Total nr of words: " ++ (show nr_words))
                                            putStrLn ("Total nr of chars: " ++ (show nr_chars))

wc :: IO ()
wc = do
      lines <- copyToOutput
      let stats = analyseLines lines
      dumpStats stats

-- 8.15
removeClutter :: String -> String
removeClutter s = [toLower c | c <- s, (isLetter c) || (isNumber c)]

palindromeChecker :: IO String 
palindromeChecker = do
                      l <- getLine
                      if l == "" then
                        return l
                      else do
                            let l' = concat $ words l
                            let cleaned = removeClutter l'
                            putStr "The text entered "
                            if (isPalindrome cleaned) == True then
                              putStrLn "is palindrome"
                            else
                              putStrLn "is not a palindrome"
                            return l

-- 8.16
preamble :: IO ()
preamble = do
            putStrLn "This program will check wether a line of entered text is a palindrome or not."
            putStrLn "Each line is terminated by pressing the return key."
            putStrLn "After each line entered you will be notified of the test's result."
            putStrLn "You can quit the program by entering an empty line."

doRepeatedlyPalindromeChecker :: IO ()
doRepeatedlyPalindromeChecker = do
                                l <- palindromeChecker
                                if l == "" then
                                  return ()
                                else
                                  doRepeatedlyPalindromeChecker

repeatedlyPalindromeChecker :: IO ()
repeatedlyPalindromeChecker = do
                                preamble
                                doRepeatedlyPalindromeChecker

-- 8.17

getIntsAcc :: [Integer] -> IO [Integer]
getIntsAcc values = do
                  value <- getInt
                  if value == 0 then
                    return values
                  else
                    getIntsAcc (value:values)

computeIntSum :: IO ()
computeIntSum = do
                  values <- getIntsAcc []
                  let result = sum values
                  putStrLn ("The sum is: " ++ (show result))
