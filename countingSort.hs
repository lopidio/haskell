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

getListIndex :: Int -> [a] -> a
getListIndex idx list = head (drop idx list)

count :: Num a => [Int] -> [a] -> [a]
count [] to = to
count (x:xs) to = count xs (addToIndex 1 x to)

accumulate :: Num t => t -> [t] -> [t]
accumulate v [] = [] 
accumulate v (x:xs) = (v+x) : (accumulate (v + x) xs)
    
sort :: [Int] -> [Int] -> [Int] -> [Int]
sort accumulatedVector [] destinationVector = destinationVector
sort accumulatedVector (x:originVector) destinationVector = 
    let
      index = getListIndex x accumulatedVector
      newDestination = addToIndex x (index - 1) destinationVector
      newAccumulatedVector = addToIndex (-1) (index - 1) accumulatedVector
    in
      sort accumulatedVector originVector newDestination

countingSort :: [Int] -> [Int]
countingSort xs = 
    let
        adjustValue = minimum (xs)
        adjustedVector = 
            if adjustValue < 0 
            then [x - adjustValue | x <- xs]
            else xs
        zeroedVec = allocate ((maximum adjustedVector) + 1)
        countVec = count adjustedVector zeroedVec
        accumulatedVector = accumulate 0 countVec
        sorted = sort accumulatedVector adjustedVector (allocate (length adjustedVector))
        readjustedVector = 
            if adjustValue < 0 
            then [x + adjustValue | x <- sorted]
            else sorted
    in 
        readjustedVector
        
