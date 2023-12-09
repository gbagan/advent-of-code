module AOC.Tuple where

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

thd3 :: (a, b, c) -> c
thd3 (a, b, c) = c