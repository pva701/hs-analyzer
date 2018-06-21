module Main where

import Universum

import Lib

-- 1. data A = A Int -- replace with newtype
-- 2  map f . map g -- replace with map (f . g)
-- 3. concat $ map f xs -- concatMap f xs
-- 4. extension
-- 5. wrong hashable

main :: IO ()
main = hsAnalize "lol.hs"
