
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
  , MutRef(..)
  , RefFor(..)
  -- * Instances
  , DefaultMutable
  , GRef
  , MutVar
  , CoerceRef
  , TraverseRef
  , GMutableRef
  -- ** Higher-Kinded Data Pattern
  , thawHKD, freezeHKD, copyHKD
  -- * Parts
  , MutPart(..)
  , withMutPart
  , freezePart, copyPart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  -- ** Built-in 'MutPart'
  , mutFst, mutSnd
  , fieldMut, Label(..)
  , posMut
  , hkdMutParts
  , coerceRef
  , mutRec
  -- ** Re-exports
  , PrimMonad, PrimState
  ) where

import           Control.Monad.Primitive
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Mutable.MutPart
import           Data.Primitive.MutVar

