collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
	| even x = x: collatz( div x 2 )
	| odd x = x: collatz(3*x + 1)

factorial :: Integral a => a -> a
factorial 1 = 1
factorial x = x * factorial (x - 1)

indexOf' :: (Eq a, Num b) => a -> [a] -> b
indexOf' _ [] = error "Empty list"
indexOf' i (x:xs) 
	| i == x = 0
	| null xs = error "Not found"
	| otherwise = 1 + indexOf' i xs

mod' :: Integral a => a -> a -> a
mod' a b
	| a < b = a
	| a == b = 0
	| otherwise = (a - b) `mod'` b

isPrime :: Integral a => a -> Bool
isPrime k = null [ x | x <- [2 .. k - 1], k `mod'`x  == 0]

extractPrimes :: Integral a => [a] -> [a]
extractPrimes xs = [x | x <- xs, isPrime x]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

contains :: Eq a => a -> [a] -> Bool
contains x [] = False
contains x (a:as)
	| x == a = True
	| otherwise = contains x as







