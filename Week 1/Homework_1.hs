-- Exercise 1 solution:

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
	| x <= 0 		= []
	| x < 10 		= [x]
	| otherwise		= (x `mod` 10):(toDigitsRev (x `div` 10))
	
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- Exercise 2 solution:

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x1:(x2:xs)) = (x1:((2*x2):(doubleEveryOtherFromLeft xs)))

doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight ls = reverse (doubleEveryOtherFromLeft (reverse ls))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls = doubleEveryOtherFromRight ls

-- Exercise 3 solution:

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs)

-- Exercise 4 solution:

validate :: Integer -> Bool
validate x = ((sumDigits (doubleEveryOther  (toDigits x))) `mod` 10) == 0

-- For Exercises 5 and 6

type Peg = String
type Move = (Peg, Peg)

-- Exercise 5 solution:

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)

-- Exercise 6 solution (I couldn't figure out optimal move combo, though):

hanoi_three :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi_three n a b c = hanoi n a b c

hanoi_four :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi_four 1 a b c d = hanoi_three 1 a b c
hanoi_four 2 a b c d = hanoi_three 2 a b c
hanoi_four n a b c d = (hanoi_four cutoff a d b c)
							++ (hanoi_three (n - cutoff) a b c )
							++ (hanoi_four cutoff d b c a)
  where
    cutoff = (n `div` 2)


