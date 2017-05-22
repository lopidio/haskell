min' :: Ord a => [a] -> a
min' [x] = x
min' (x:xs) = min x (min' xs)

deleteFirst :: Eq t => t -> [t] -> [t]
deleteFirst _ [] = [] 
deleteFirst a (b:bc) | a == b    = bc 
                     | otherwise = b : deleteFirst a bc

selectionSort :: (Ord a) => [a] -> [a]  
selectionSort [] = []
selectionSort xs = 
    let
        smallest = min' xs
        smallestComplement = deleteFirst smallest xs
    in
        smallest : selectionSort smallestComplement
