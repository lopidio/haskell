
allocate :: Num a => Int -> [a]
allocate x = take x (repeat 0)


addToIndex :: Num a => a -> Int -> [a] -> [a]
addToIndex value idx list =
	let
		front = take idx list
		element = head (drop idx list)
		back = drop (succ idx) list
	in
		front ++ (value + element) : back	

count :: Num a => [Int] -> [a] -> [a]
count [] to = to
count (x:xs) to = count xs (addToIndex 1 x to)

accumulate v (x:[]) = [v + x] 
accumulate v (x:xs) = (v+x):accumulate (v + x) xs
	

countingSort :: Num a => [Int] -> [a]
countingSort xs = 
	let
		zeroedVec = allocate ((maximum xs) + 1)
		countVec = count xs zeroedVec
--		accumulate = 
	in 
		countVec
