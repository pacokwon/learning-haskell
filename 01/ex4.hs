toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev $ n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = fst $ foldr (\x (acc, bool) ->
    ((if bool then x * 2 else x) : acc, not bool)) ([], False) xs

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (\x s -> s + x `div` 10 + x `mod` 10) 0 xs

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `div` 10 == 0
