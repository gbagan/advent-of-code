-- https://adventofcode.com/2019/day/22
module Day22 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, choice, signedDecimal, sepEndBy1, eol, scanf)
import           Data.Mod (Mod, unMod)

data Technique = NewStack | Cut Integer | Increment Integer
data AffineFn n = AffineFn !n !n

parser :: Parser [Technique]
parser = technique `sepEndBy1` eol where
    technique = choice [ NewStack <$"deal into new stack"
                       , [scanf|$Cut cut {signedDecimal}|]
                       , [scanf|$Increment deal with increment {signedDecimal}|]
                       ]

instance Num n => Semigroup (AffineFn n) where
    AffineFn a2 b2 <> AffineFn a1 b1 = AffineFn (a2 * a1) (a2 * b1 + b2)
 
instance Num n => Monoid (AffineFn n) where
    mempty = AffineFn 1 0

invert :: Fractional n => AffineFn n -> AffineFn n
invert (AffineFn a b) = AffineFn a' b' where
    a' = 1/a
    b' = - (a' * b)

techniqueToAffineFn :: KnownNat n => Technique -> AffineFn (Mod n)
techniqueToAffineFn NewStack = AffineFn (-1) (-1)
techniqueToAffineFn (Cut m) = AffineFn 1 (-(fromInteger m))
techniqueToAffineFn (Increment m) = AffineFn (fromInteger m) 0

shuffleToAffineFn :: KnownNat n => [Technique] -> AffineFn (Mod n)
shuffleToAffineFn  = foldMap techniqueToAffineFn . reverse

apply :: Num n => AffineFn n -> n -> n
apply (AffineFn a b) x = a * x + b

part1 :: [Technique] -> Integer
part1 shuffle = fromIntegral . unMod $ f `apply` 2019 where
    f = shuffleToAffineFn shuffle :: AffineFn (Mod 10_007)

part2 :: [Technique] -> Integer
part2 shuffle = fromIntegral . unMod $ invert f' `apply` 2020 where
    f = shuffleToAffineFn shuffle :: AffineFn (Mod 119_315_717_514_047)
    f' = stimes (101_741_582_076_661 :: Int) f

solve :: Text -> IO ()
solve = aoc parser part1 part2