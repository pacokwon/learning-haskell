fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : nxt
    where nxt = 1 : zipWith (+) fibs2 nxt -- to me, still seems like magic

data Stream a = Cons a (Stream a)
streamToList :: Stream a -> [a]
streamToList (Cons elt stream) = elt : streamToList stream

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x strm) = Cons (f x) (streamMap f strm)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed transform seed = Cons seed (streamFromSeed transform (transform seed))

nats :: Stream Integer
nats = Cons 1 (streamMap (1 +) nats)

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons eltA restA) strmB = Cons eltA (interleaveStream strmB restA)

ruler :: Stream Integer
ruler = interleaveStream (streamRepeat 0) (streamMap (+ 1) ruler)
