sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (\x s -> s + x `div` 10 + x `mod` 10) 0 xs
