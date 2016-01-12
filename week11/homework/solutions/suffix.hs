suffix :: (Eq a) => [a] -> [a] -> Bool
suffix sfx ls = sfx == drop (length ls - length sfx) ls 
