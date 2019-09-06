{- Haskell solution to Towers of Hanoi -}

type Peg = String
type Move = (Peg, Peg)
-- type declarations

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- function that returns a list of moves for performing towers of hanoi
-- on n number of discs

hanoi n a b c
  | n <= 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a
