ans = sumOfSquares - squareOfSum
  where 
    sumOfSquares = sum $ map (^2) [1..100] 
    squareOfSum = sum [1..100] ^ 2
