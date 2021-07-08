xor :: [Bool] -> Bool
xor = foldr (\isOdd x -> if x then not isOdd else isOdd) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []
