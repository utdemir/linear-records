{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

import Gauge.Main
import Data.Function ((&))
import Data.List (foldl')
import qualified Prelude.Linear as L
import qualified Unsafe.Linear as Unsafe
import Data.Record.Linear (Rec, type (~~), withRec, writeRec, readRec)

data ExampleData
  = ExampleData
      { ed0 :: Int
      , ed1 :: Int
      , ed2 :: Int
      , ed3 :: Int
      , ed4 :: Int
      , ed5 :: Int
      , ed6 :: Int
      , ed7 :: Int
      , ed8 :: Int
      , ed9 :: Int
      }
  deriving Show

type ExampleRec
  = [ "er0" ~~ Int
    , "er1" ~~ Int
    , "er2" ~~ Int
    , "er3" ~~ Int
    , "er4" ~~ Int
    , "er5" ~~ Int
    , "er7" ~~ Int
    , "er8" ~~ Int
    , "er9" ~~ Int
    ]

flipExampleData :: ExampleData -> ExampleData
flipExampleData ed =
  ed5 ed & \i ->
    let n = i + 1
    in  n `seq` ed { ed5 = n }

flipExampleRec :: Rec ExampleRec #-> Rec ExampleRec
flipExampleRec er =
  readRec @"er5" er L.& \(L.Unrestricted f5, r') ->
    writeRec @"er5" (f5 + 3) r'

flipManyExampleData :: Int -> Int
flipManyExampleData numIters =
  ed5 $
    foldl'
      (\d _ -> flipExampleData d)
      (ExampleData 0 0 0 0 0 0 0 0 0 0)
      [0..numIters]

flipManyExampleRec :: Int -> Int
flipManyExampleRec numIters =
  withRec @ExampleRec 0 0 0 0 0 0 0 0 0 go
    & \(L.Unrestricted i) -> i
  where
    go :: Rec ExampleRec #-> L.Unrestricted Int
    go rec =
      lfoldl'
        (\r i -> i `L.lseq` flipExampleRec r)
        rec
        [0..numIters]
        L.& readRec @"er5"
        L.& \(i, r) -> r `L.lseq` i

lfoldl' :: (b #-> a #-> b) -> b #-> [a] #-> b
lfoldl' _ b [] = b
lfoldl' f b (x:xs) = lforce (\r -> lfoldl' f r xs) (f b x)

lforce :: (a #-> b) #-> a #-> b
lforce f = Unsafe.toLinear2 (Prelude.$!) (L.forget f)

main :: IO ()
main = defaultMain [
  bgroup "flip" [ bench "data" $ nf flipManyExampleData numIters
                , bench "rec" $ nf flipManyExampleRec numIters
                ]
  ]
  where
   numIters = 2 ^ (24 :: Int)


