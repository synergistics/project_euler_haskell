-- Problem 5: Find the smallest number that is a multiple of all numbers 1 to 20
import Data.List
import EulerUtil.Primes

smallestMultiple x = product xs
  where xs = foldr addNecessary [] $ map primeFactors [2..x]
        addNecessary xs ys = xs ++ (ys \\ xs)

ans = smallestMultiple 20
