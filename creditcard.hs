toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [(n `mod` 10)]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs)
  | (length zs) `mod` 2 == 0 = (2*x:y:doubleEveryOther zs)
  | (length zs) `mod` 2 /= 0 = (x:2*y:doubleEveryOther zs)