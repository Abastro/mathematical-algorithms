module PolyFast where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.List
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MG
import GHC.Base

plusPoly ::
  (G.Vector v a) =>
  (a -> a -> a) ->
  v a ->
  v a ->
  v a
plusPoly add xs ys = runST $ do
  let lenXs = G.length xs
      lenYs = G.length ys
      lenMn = lenXs `min` lenYs
      lenMx = lenXs `max` lenYs

  zs <- MG.unsafeNew lenMx
  forM_ [0 .. lenMn - 1] $ \i ->
    MG.unsafeWrite zs i (add (G.unsafeIndex xs i) (G.unsafeIndex ys i))
  G.unsafeCopy
    (MG.unsafeSlice lenMn (lenMx - lenMn) zs)
    (G.unsafeSlice lenMn (lenMx - lenMn) (if lenXs <= lenYs then ys else xs))

  G.unsafeFreeze zs
{-# INLINEABLE plusPoly #-}

karatsubaThreshold :: Int
karatsubaThreshold = 32

karatsuba ::
  (G.Vector v a) =>
  a ->
  (a -> a -> a) ->
  (a -> a -> a) ->
  (a -> a -> a) ->
  v a ->
  v a ->
  v a
karatsuba zer add sub mul = go
  where
    conv = inline convolution zer add mul
    go xs ys
      | lenXs <= karatsubaThreshold || lenYs <= karatsubaThreshold =
          conv xs ys
      | otherwise = runST $ do
          zs <- MG.unsafeNew lenZs
          forM_ [0 .. lenZs - 1] $ \k -> do
            let z0 =
                  if k < G.length zs0
                    then G.unsafeIndex zs0 k
                    else zer
                z11 =
                  if k - m >= 0 && k - m < G.length zs11
                    then G.unsafeIndex zs11 (k - m)
                    else zer
                z10 =
                  if k - m >= 0 && k - m < G.length zs0
                    then G.unsafeIndex zs0 (k - m)
                    else zer
                z12 =
                  if k - m >= 0 && k - m < G.length zs2
                    then G.unsafeIndex zs2 (k - m)
                    else zer
                z2 =
                  if k - 2 * m >= 0 && k - 2 * m < G.length zs2
                    then G.unsafeIndex zs2 (k - 2 * m)
                    else zer
            MG.unsafeWrite zs k (z0 `add` (z11 `sub` (z10 `add` z12)) `add` z2)
          G.unsafeFreeze zs
      where
        lenXs = G.length xs
        lenYs = G.length ys
        lenZs = lenXs + lenYs - 1

        m = ((lenXs `min` lenYs) + 1) `shiftR` 1

        xs0 = G.slice 0 m xs
        xs1 = G.slice m (lenXs - m) xs
        ys0 = G.slice 0 m ys
        ys1 = G.slice m (lenYs - m) ys

        xs01 = plusPoly add xs0 xs1
        ys01 = plusPoly add ys0 ys1
        zs0 = go xs0 ys0
        zs2 = go xs1 ys1
        zs11 = go xs01 ys01
{-# INLINEABLE karatsuba #-}

convolution ::
  (G.Vector v a) =>
  a ->
  (a -> a -> a) ->
  (a -> a -> a) ->
  v a ->
  v a ->
  v a
convolution zer add mul = \xs ys ->
  let lenXs = G.length xs
      lenYs = G.length ys
      lenZs = lenXs + lenYs - 1
   in if lenXs == 0 || lenYs == 0
        then G.empty
        else G.generate lenZs $ \k ->
          foldl'
            (\acc i -> acc `add` mul (G.unsafeIndex xs i) (G.unsafeIndex ys (k - i)))
            zer
            [max (k - lenYs + 1) 0 .. min k (lenXs - 1)]
{-# INLINEABLE convolution #-}
