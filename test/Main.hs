{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Data.Record.Linear
import Data.Function ((&))
import qualified Prelude.Linear as L
import qualified Control.Optics.Linear.Internal as L
import Prelude.Linear (Unrestricted(..))
import qualified Data.Functor.Linear as Data

type MyRec =
  [ "field1" ~~ Int
  , "field2" ~~ String
  , "field3" ~~ Bool
  ]

processRec :: Rec MyRec #-> Unrestricted (Int, String, Bool)
processRec r =
  r
    L.& L.over (lensRec @"field1") (Data.fmap (L.+ 1))
    L.& writeRec @"field2" "Demir"
    L.& L.over (lensRec @"field3") (Data.fmap L.not)
    L.&
      readRec @"field1" L.& \(Unrestricted f1, r') ->
        readRec @"field2" r' L.& \(Unrestricted f2, r'') ->
          readRec @"field3" r'' L.& \(Unrestricted f3, r''') ->
            r''' `L.lseq` Unrestricted (f1, f2, f3)

prop_processRec :: Property
prop_processRec = withTests 1 . property $ do
  withRec @MyRec 1 "Utku" False processRec & \case
    Unrestricted res -> res === (2, "Demir", True)

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
