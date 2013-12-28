module Main where

import Acme.Dont

main :: IO ()
main = don't $ do 
  return ()
  return ()
