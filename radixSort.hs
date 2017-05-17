import Data.Bits

and' :: (Num a, Bits a) => a -> a -> a
and' x y = x .&. y

orderBitEven :: (Integral b, Bits b) => Int -> b -> Bool
orderBitEven i b = even (shiftR b i)
	
radix :: (Integral b, Bits b) => Int -> [b] -> [b]
radix i xs = 
	let
		evens = [x | x <- xs, orderBitEven i x]
		odds = [x | x <- xs, not (orderBitEven i x)]
		stopCondition = null [x | x <- xs, (shiftR x i) > 0]
	in
		if stopCondition
		then evens ++ odds
		else radix (i + 1) (evens ++ odds)

radixSort xs = radix 0 xs
