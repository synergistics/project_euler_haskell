-- Problem 2: Find the sum of the even fibonacci numbers up to 4000000
fibs = 1 : 1 : zipWith (+) fibs (tail fibs) 

ans = sum $ filter even (takeWhile (< 4000000) fibs)
