difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = (sum [num|num<-[1..n]]) ^ 2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum [num ^ 2 | num<-[1..n]] 
