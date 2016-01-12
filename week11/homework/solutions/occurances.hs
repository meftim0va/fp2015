occurrences :: [Int] -> [Int] -> [Int]
occurrences l1 l2 = map (\x -> length (filter (==x) l2)) l1
