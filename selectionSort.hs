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
