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

	
quickSort :: (Ord a) => [a] -> [a]  
quickSort [] = []
quickSort (x:xs) =
    let 
        smallers = quickSort [s | s <- xs, s <= x]
        greaters = quickSort [s | s <- xs, s > x]
    in
        smallers ++ [x] ++ greaters

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
  | x <= y = x: (merge xs (y:ys))
  | otherwise = y: (merge (x:xs) ys)

takeHalf :: [a] -> [a]
takeHalf xs = take (div (length' xs) 2) xs
dropHalf :: [a] -> [a]
dropHalf xs = drop (div (length' xs) 2) xs

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []  
mergeSort (x:[]) = [x]
mergeSort xs = 
    let firstHalf = mergeSort (takeHalf xs)
        secondHalf = mergeSort (dropHalf xs)
    in  merge firstHalf secondHalf  


min' :: Ord a => [a] -> a
min' [x] = x
min' (x:xs) = min x (min' xs)

removeItemFromList :: Eq a => a -> [a] -> [a]
removeItemFromList item xs = [ x | x <- xs, x /= item ]

selectionSort :: (Ord a) => [a] -> [a]  
selectionSort [] = []
selectionSort xs = 
    let
        smallest = min' xs
        smallestComplement = removeItemFromList smallest xs
    in
        smallest : selectionSort smallestComplement

insertInSortedList :: (Ord a) => a -> [a] -> [a]  
insertInSortedList x xs =
    let 
        smallers = [s | s <- xs, s <= x]
        greaters = [s | s <- xs, s > x]
    in
        smallers ++ [x] ++ greaters

insertionRecursion :: (Ord a) => [a] -> [a] -> [a]
insertionRecursion s [] = s
insertionRecursion ls (x:xs) = insertionRecursion (insertInSortedList x ls) xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort xs = insertionRecursion [] xs

