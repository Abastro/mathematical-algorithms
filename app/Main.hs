module Main (main) where

import Data.Vector qualified as V
import Poly

main :: IO ()
main = do
  let f :: Poly Int = Poly (V.fromList [1, 2, 3])
  let g :: Poly Int = Poly (V.fromList [4, 5])
  putStrLn $ "f: " <> show f <> ", g: " <> show g
  putStrLn $ "f + g: " <> show (f + g)
  putStrLn $ "f * g: " <> show (f * g)
  putStrLn $ "Karatsuba f * g: " <> show (normalize $ karatsubaMult f g)
