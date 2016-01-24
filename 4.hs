isPalindrome x = show x == (reverse $ show x)
ans = maximum [p | a <- [999,998..100], b <- [999,998..100], let p = a * b, isPalindrome p]
