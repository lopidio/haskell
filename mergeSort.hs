length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

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


