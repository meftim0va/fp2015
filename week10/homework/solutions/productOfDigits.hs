productOfDigits :: Int -> Int
productOfDigits n
    | n < 10 = n
    | otherwise = productOfDigits (n `div` 10) * (n `mod` 10)
