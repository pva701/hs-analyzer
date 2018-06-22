{-# LANGUAGE Rank2Types #-}

module Main where

import           Universum

import           Lib

-- 1. data A = A Int -- replace with newtype
-- 2  map f . map g -- replace with map (f . g)

main :: IO ()
main = hsAnalize "lol.hs"
