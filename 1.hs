-- Problem 1: Find the sum of the natural numbers under 1000 divisible by 3 or 5
divisibleBy :: (Integral a) => a -> a -> Bool
divisibleBy x y = x `mod` y == 0

ans = sum $ filter divisibleBy3or5 [1..999]
  where divisibleBy3or5 = \x -> (divisibleBy x 3) || (divisibleBy x 5)
