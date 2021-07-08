exclude :: Integer -> [Integer]
exclude n = [i + j + 2 * i * j | i <- [1 .. n], j <- [i .. n]]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let excludeCand = exclude n in
                    2:[2 * x + 1 | x <- [1 .. n], x `notElem` excludeCand]
