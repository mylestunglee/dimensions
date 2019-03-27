module Ratio ((^%)) where

import Data.Ratio ((%), numerator, denominator)
import Data.Maybe (listToMaybe)

infixr 8 ^%
(^%) :: Rational -> Rational -> Maybe Rational
(^%) b e
  | e < 0            = (1 / b) ^% (-e)
  | b == 0 && e == 0 = Nothing
  | otherwise        = fmap (%) x <*> y
  where
    x = root' bn
    y = root' bd
    root' :: Integer -> Maybe Integer
    root' z = fmap (^ en) (root z ed)
    bn = numerator b
    bd = denominator b
    en = numerator e
    ed = denominator e

root :: Integer -> Integer -> Maybe Integer
root b e
  = elemIndex b (takeWhile (<= b) (map (^ e) [0..]))

elemIndex :: Eq a => a -> [a] -> Maybe Integer
elemIndex x ys
  = listToMaybe [i | (y, i) <- zip ys [0..], x == y]
