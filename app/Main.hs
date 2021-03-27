module Main where

import Lib

f = X `Mul` X `Add` Sin X

main :: IO ()
main = do
  print $grad f
  print $trans $grad f
  print $eval 2 f
