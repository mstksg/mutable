
-- |
-- Module      : Data.Mutable.Class
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Main entrypoint of the package.  Abstract over different types for
-- piecewise-mutable references of values.
--
-- See <https://mutable.jle.im/> for a comprehensive introduction.
module Data.Mutable (
    Mutable(..)
  , modifyRef, modifyRef'
  , updateRef, updateRef'
  , RefFor(..)
  -- * Instances
  , DefaultMutable
  , GRef
  , MutVar
  , CoerceRef(..)
  , TraverseRef(..)
  , GMutableRef(..)
  , HListRef(..)
  -- * Providing/overriding instances
  , VarMut(..)
  , CoerceMut(..)
  , TraverseMut(..)
  , Immutable(..)
  -- * Parts
  , MutPart(..)
  , withMutPart
  , freezePart, copyPart
  , movePartInto, movePartOver, movePartWithin
  , clonePart, unsafeFreezePart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  -- * Built-in 'MutPart'
  , mutFst, mutSnd
  -- ** Field
  , FieldMut(..), withField, mutField, Label(..)
  -- ** Position
  , PosMut(..), withPos, mutPos
  -- ** HList
  , ListMut(..), withListMut
  -- ** Other
  , hkdMutParts, HKDMutParts
  , mutRec
  , coerceRef, withCoerceRef
  -- ** Re-exports
  , PrimMonad, PrimState
  ) where

import           Control.Monad.Primitive
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Mutable.Parts
import           Data.Primitive.MutVar

