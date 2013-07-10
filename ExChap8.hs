module ExChap8 where

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

              

