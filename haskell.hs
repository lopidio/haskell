collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
	| even x = x: collatz( div x 2 )
	| odd x = x: collatz(3*x + 1)

factorial :: Integral a => a -> a
factorial 1 = 1
factorial x = x * factorial (x - 1)

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

indexOf' :: (Eq a, Num a) => a -> [t] -> t
indexOf' _ [] = error "Empty list"
indexOf' 0 (x:xs) = x
indexOf' i (x:xs) = indexOf' (i -1) xs

mod' :: Integral a => a -> a -> a
mod' a b
	| a < b = a
	| a == b = 0
	| otherwise = (a - b) `mod'` b

isPrime :: Integral a => a -> Bool
isPrime k = null [ x | x <- [2 .. k - 1], k `mod'`x  == 0]

extractPrimes :: Integral a => [a] -> [a]
extractPrimes xs = [x | x <- xs, isPrime x]

