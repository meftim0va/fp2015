containsDigits :: Int -> Int -> Bool
containsDigits a b
    | b < 10    = containsDigits' a b
    | otherwise = containsDigits' a (b `mod` 10) && containsDigits a (b `div` 10)
        where
            containsDigits' n digit 
                | n < 10    = n == digit
                | otherwise = n `mod` 10 == digit || containsDigits' (n `div` 10) digit
