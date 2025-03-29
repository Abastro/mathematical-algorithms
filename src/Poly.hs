module Poly where

import Data.List
import Data.Maybe
import Data.Vector.Unboxed qualified as V

-- Zip two vectors while padding 0s on the shorter vector.
vecZipPad0With :: (V.Unbox a, Num a) => (a -> a -> a) -> V.Vector a -> V.Vector a -> V.Vector a
vecZipPad0With f xs ys = V.generate (max (V.length xs) (V.length ys)) $
  \i -> fromMaybe 0 (xs V.!? i) `f` fromMaybe 0 (ys V.!? i)

-- | Polynomial type.
--
-- >>> Poly (V.fromList [1 :: Int .. 5])
-- 1 X^0 + 2 X^1 + 3 X^2 + 4 X^3 + 5 X^4

-- >>> Poly (V.fromList [1 :: Int, 2]) * Poly (V.fromList [3 :: Int, 4, 5])
-- 3 X^0 + 10 X^1 + 13 X^2 + 10 X^3

-- >>> Poly (V.fromList [1 :: Int, 2]) * Poly (V.fromList [])
-- 0 X^0
newtype Poly a = Poly (V.Vector a)
  deriving (Eq)

unwrapPoly :: Poly a -> V.Vector a
unwrapPoly (Poly v) = v

makePoly :: (V.Unbox a) => [a] -> Poly a
makePoly = Poly . V.fromList

-- | Degree, assuming top term is nonzero
degree :: (V.Unbox a) => Poly a -> Int
degree (Poly f) = V.length f - 1

-- | Shift up polynomial by X^n
shiftUp :: (V.Unbox a, Num a) => Int -> Poly a -> Poly a
shiftUp n (Poly f) = Poly $ V.replicate n 0 <> f

-- | Shift down polynomial by X^n
shiftDown :: (V.Unbox a) => Int -> Poly a -> Poly a
shiftDown n (Poly f) = Poly $ V.drop n f

-- | Remainder under X^n
remXn :: (V.Unbox a) => Int -> Poly a -> Poly a
remXn n (Poly f) = Poly $ V.take n f

-- | Normalize polynomial, removing leading 0s
--
-- >>> normalize $ Poly (V.fromList [1 :: Int, 0, 0])
-- 1 X^0
--
-- >>> normalize $ Poly (V.fromList [1 :: Int, 2, 3, 0])
-- 1 X^0 + 2 X^1 + 3 X^2
normalize :: (Eq a, Num a, V.Unbox a) => Poly a -> Poly a
normalize (Poly f) = Poly remain
  where
    (_, remain) = V.spanR (== 0) f

-- | This Num instance implements the classical multiplication.
instance (Num a, V.Unbox a) => Num (Poly a) where
  (+) :: Poly a -> Poly a -> Poly a
  Poly f + Poly g = Poly $ vecZipPad0With (+) f g
  (-) :: Poly a -> Poly a -> Poly a
  Poly f - Poly g = Poly $ vecZipPad0With (-) f g
  (*) :: Poly a -> Poly a -> Poly a
  Poly f * Poly g = sum (map Poly mults)
    where
      mults = zipWith (\i fi -> V.map (fi *) (V.replicate i 0 <> g)) [0 ..] (V.toList f)
  negate :: Poly a -> Poly a
  negate (Poly f) = Poly $ V.map negate f
  abs :: Poly a -> Poly a
  abs = error "abs: invalid on poly"
  signum :: Poly a -> Poly a
  signum = error "signum: invalid on poly"
  fromInteger :: Integer -> Poly a
  fromInteger = Poly . V.singleton . fromInteger

instance (V.Unbox a, Show a) => Show (Poly a) where
  show (Poly p) = intercalate " + " $ zipWith (\i coeff -> show coeff <> " X^" <> show i) [0 :: Int ..] (V.toList p)

karatsubaMult :: (Num a, V.Unbox a) => Poly a -> Poly a -> Poly a
karatsubaMult a b = atLog degBound a b
  where
    degBound = fromJust $ find (> max (degree a) (degree b)) [2 ^ i | i <- [0 :: Int ..]]

    -- degBnd: power-of-two degree bound
    atLog degBnd f g = case degBnd of
      1 -> f * g
      _ -> shiftUp degBnd prod1 + shiftUp nextBound (prodAdd - prod0 - prod1) + prod0
      where
        nextBound = degBnd `div` 2
        f1 = shiftDown nextBound f
        f0 = remXn nextBound f
        g1 = shiftDown nextBound g
        g0 = remXn nextBound g
        prod0 = atLog nextBound f0 g0
        prod1 = atLog nextBound f1 g1
        prodAdd = atLog nextBound (f0 + f1) (g0 + g1)
