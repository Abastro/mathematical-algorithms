module Main (main) where

import Control.Exception
import Poly
import System.Random
import System.TimeIt
import Control.Monad
import PolyFast

main :: IO ()
main = do
  do
    let f :: Poly Int = makePoly [1, 2, 3]
    let g :: Poly Int = makePoly [4, 5]
    putStrLn $ "f: " <> show f <> ", g: " <> show g
    putStrLn $ "f + g: " <> show (f + g)
    putStrLn $ "Naive f * g: " <> show (f * g)
    putStrLn $ "Karatsuba f * g: " <> show (normalize $ karatsubaMult f g)

  putStrLn ""
  -- experimentFor 250
  -- experimentFor 500
  -- experimentFor 1000
  -- karatsubaFor 2000
  -- karatsubaFor 4000

  fastKaratsubaFor 512
  fastKaratsubaFor 1024
  fastKaratsubaFor 2048
  fastKaratsubaFor 4096
  fastKaratsubaFor 8192
  fastKaratsubaFor 16384
  fastKaratsubaFor 32768
  fastKaratsubaFor 65536
  where
    experimentFor n = do
      setStdGen $ mkStdGen 10
      let randomPoly size = makePoly <$> replicateM size (randomRIO (-100, 100))
      putStrLn $ "Size " <> show n
      f :: Poly Int <- randomPoly n
      g :: Poly Int <- randomPoly n
      putStrLn "naive:"
      _ <- timeIt $ evaluate (f * g)
      putStrLn "Karatsuba:"
      _ <- timeIt $ evaluate (karatsubaMult f g)
      putStrLn "Finished"

    karatsubaFor n = do
      setStdGen $ mkStdGen 10
      let randomPoly size = makePoly <$> replicateM size (randomRIO (-100, 100))
      putStrLn $ "Size " <> show n
      f :: Poly Int <- randomPoly n
      g :: Poly Int <- randomPoly n
      putStrLn "Karatsuba:"
      _ <- timeIt $ evaluate (karatsubaMult f g)
      putStrLn "Finished"

    fastKaratsubaFor n = do
      setStdGen $ mkStdGen 10
      let randomPoly size = makePoly <$> replicateM size (randomRIO (-100, 100))
      putStrLn $ "Size " <> show n
      f :: Poly Int <- randomPoly n
      g :: Poly Int <- randomPoly n
      putStrLn "Fast Karatsuba:"
      _ <- timeIt $ evaluate (karatsuba 0 (+) (-) (*) (unwrapPoly f) (unwrapPoly g))
      putStrLn "Finished"
