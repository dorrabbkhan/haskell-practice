{- Haskell file that validates credit card numbers. For a credit card
number to be valid, every second number from the right must be doubled.
Then the individual digits of all the numbers are added, and the returned
sum must be a multiple of 10 for the credit card number to be valid. -}

toDigits :: Integer -> [Integer]
-- converts an integer to a list of its individual digits
-- e.g. 12345 -> [1,2,3,4,5]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]

toDigitsRev :: Integer -> [Integer]
-- converts an integer to a list of its individual digits, reversed
-- e.g. 12345 -> [5,4,3,2,1]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
-- doubles every other number from the right of a list
-- e.g. [5,4,3,2,1] -> [5,8,3,4,1] and [1,2,3,4] -> [2,2,6,4]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs)
  | (length zs) `mod` 2 == 0 = (2*x:y:doubleEveryOther zs)
  | (length zs) `mod` 2 /= 0 = (x:2*y:doubleEveryOther zs)

sumDigits :: [Integer] -> Integer
-- returns the sum of all digits of all numbers in a list
-- e.g. [1,10,15,6,2] -> 16 since 1+1+0+1+5+6+2 = 16
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + sumDigits xs
  | otherwise = x `mod` 10 + x `div` 10 + sumDigits xs

validate :: Integer -> Bool
-- validates if a credit card number is valid or not
validate n
  | n >= 1000000000000000 && n <= 9999999999999999 = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0
  | otherwise = False