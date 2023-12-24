    module AOC.Tuple where

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c