
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
module Data.Mutable (
    Mutable(..)
  , modifyRef, modifyRef'
  , updateRef, updateRef'
  , RefFor(..)
  -- * Instances
  , DefaultMutable
  , GRef
  , MutVar
  , CoerceRef
  , TraverseRef
  , GMutableRef
  -- * Providing/overriding instances
  , VarMut(..)
  , CoerceMut(..)
  , TraverseMut(..)
  , Immutable(..)
  -- * Parts
  , MutPart(..)
  , withMutPart
  , freezePart, copyPart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  -- ** Built-in 'MutPart'
  , mutFst, mutSnd
  , fieldMut, withField, mutField, Label(..)
  , posMut, withPos, mutPos
  , coerceRef, withCoerceRef
  , hkdMutParts
  , mutRec
  -- ** Re-exports
  , PrimMonad, PrimState
  ) where

import           Control.Monad.Primitive
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Mutable.Parts
import           Data.Primitive.MutVar

