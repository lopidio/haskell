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
