interestingNumber :: Int -> Bool
interestingNumber n
    | n <= 1    = False
    | otherwise = n == sumOfDivisors (sumOfDivisors n 1) 1
        where
            sumOfDivisors n i
                | n == i         = 0
                | n `mod` i == 0 = i + sumOfDivisors n (i + 1)
                | otherwise      = sumOfDivisors n (i + 1)
