doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = fst $ foldr (\x (acc, bool) ->
    ((if bool then x * 2 else x) : acc, not bool)) ([], False) xs
