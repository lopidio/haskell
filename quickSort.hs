quickSort :: (Ord a) => [a] -> [a]  
quickSort [] = []
quickSort (x:xs) =
    let 
        smallers = quickSort [s | s <- xs, s <= x]
        greaters = quickSort [s | s <- xs, s > x]
    in
        smallers ++ [x] ++ greaters
