{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Record.Linear
  ( Rec
  , type (~~)
  , withRec
  , readRec
  , unsafeReadRec
  , writeRec
  , unsafeWriteRec
  , lensRec
  ) where

import Data.Int
import Data.Proxy
import qualified Prelude as Prelude
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Functor.Linear as Data
import Prelude.Linear
import Control.Optics.Linear.Internal
import Data.Profunctor.Linear
import GHC.TypeLits
import Data.Kind
import qualified Unsafe.Linear as Unsafe
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array

newtype Rec (a :: [Type]) =
  Rec (Array ())
  deriving newtype Consumable

data (~~) (name :: Symbol) (value :: Type)

-- Building a record

withRec
  :: forall fields a ret.
   ( WithRec
       fields
       ret
   , ret ~ ((Rec fields #-> Unrestricted a) -> Unrestricted a)
   )
  => WithRecParams fields ret
withRec
  = withRecArr @fields @ret
      (\l f -> Array.fromList l (f . Rec)
      )

class WithRec (fields :: [Type]) (ret :: Type) where
  type WithRecParams fields ret
  withRecArr :: ([()] -> ret) -> WithRecParams fields ret

instance WithRec '[] ret where
  type WithRecParams '[] ret = ret
  withRecArr f = f []

instance WithRec rest ret => WithRec (name ~~ ty ': rest) ret where
  type WithRecParams (name ~~ ty ': rest) ret = ty -> WithRecParams rest ret
  withRecArr f ty = withRecArr @rest (\arr -> f (unsafeCoerce @ty @() ty : arr))

-- Information about a field inside a record
type HasField fields name info =
  ( RecField fields name ~ info
  , KnownNat (FieldInfoOffset info)
  )

data FieldInfo (offset :: Nat) (ty :: Type)

type family FieldInfoOffset t where FieldInfoOffset (FieldInfo t _) = t
type family FieldInfoType t where FieldInfoType (FieldInfo _ t) = t

type family FieldInfoAddOffset n t where
  FieldInfoAddOffset n (FieldInfo t a) = FieldInfo (t + n) a

type family RecField fields name where
  RecField '[] name
    = TypeError ('Text "Field not found: " ':<>: 'Text name)
  RecField (name ~~ value ': rest) name
    = FieldInfo 0 value
  RecField (name' ~~ value ': rest) name
    = FieldInfoAddOffset 1 (RecField rest name)

-- Accesing fields

unsafeReadRec :: forall a b. Int -> Rec b #-> (Unrestricted a, Rec b)
unsafeReadRec offset (Rec arr) =
  Array.read arr offset & \case
    (arr', ua) ->
      ( Unsafe.coerce @() @a Data.<$> ua
      , Rec arr'
      )
{-# INLINE unsafeReadRec #-}

readRec :: forall name fields info. HasField fields name info
        => Rec fields #-> (Unrestricted (FieldInfoType info), Rec fields)
readRec =
  let offset = fromIntegral Prelude.$ natVal @(FieldInfoOffset info) Proxy
  in  unsafeReadRec offset
{-# INLINE readRec #-}

unsafeWriteRec :: forall a b. Int -> a -> Rec b #-> Rec b
unsafeWriteRec offset val (Rec arr) =
  Rec (Array.write arr offset (Unsafe.coerce @a @() val))
{-# INLINE unsafeWriteRec #-}

writeRec :: forall name fields info. HasField fields name info
         => FieldInfoType info -> Rec fields #-> Rec fields
writeRec value =
  let offset = fromIntegral Prelude.$ natVal @(FieldInfoOffset info) Proxy
  in  unsafeWriteRec offset value
{-# INLINE writeRec #-}

unsafeLens :: Int -> Lens' (Unrestricted a) (Rec b)
unsafeLens i = Optical $ \f ->
  dimap
    (unsafeReadRec i)
    (\(Unrestricted b, r) -> unsafeWriteRec i b r)
    (first f)
{-# INLINE unsafeLens #-}

lensRec :: forall name fields info. HasField fields name info
        => Lens' (Unrestricted (FieldInfoType info)) (Rec fields)
lensRec =
  let offset = fromIntegral Prelude.$ natVal @(FieldInfoOffset info) Proxy
  in  unsafeLens offset
{-# INLINE lensRec #-}
----------------------------------------------------------------------

{-data Handle = Handle

openFile :: FilePath #-> Handle
openFile = undefined

closeFile :: Handle #-> ()
closeFile = undefined

readFile' :: Handle #-> (String, Handle)
readFile' = undefined

readFileContents :: String -> String
readFileContents path =
  openFile & \handle ->
    readFile handle & \(str, handle') ->
      str
-}

