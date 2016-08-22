module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Input:"
  print input

  putStrLn ""
  putStrLn "Scanned:"
  print $ scanning input

  putStrLn ""
  putStrLn "Final output:"
  print $ algorithm input
