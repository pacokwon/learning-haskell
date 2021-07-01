type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to tmp
  | n <= 1 = [(from, to)]
  | otherwise = (hanoi (n - 1) from tmp to) ++ [(from, to)] ++ (hanoi (n - 1) tmp to from)
