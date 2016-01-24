module EulerUtil.Primes
( primes
, isPrime
, primeFactors
) where 

minus (x:xs) (y:ys) = case compare x y of
  LT -> x : minus xs (y:ys)
  EQ -> minus xs ys
  GT -> minus (x:xs) ys

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x y = x `mod` y == 0
  
primes :: [Int]
primes = eratos [2..]
  where
    eratos (x:xs) = x : eratos (xs `minus` [x,x+x..]) 

isPrime :: Int -> Bool
isPrime x = elem x (takeWhile (<= x) primes)

primeFactors :: Int -> [Int]
primeFactors x
  | isPrime x = [x]
  | otherwise = left : primeFactors right
  where
    left = head $ filter (isDivisibleBy x) (takeWhile (<= (div x 2)) primes)
    right = div x left

 
