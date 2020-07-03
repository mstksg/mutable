
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
  , RecRef(..)
  , HListRef(..)
  , UnitRef(..)
  , VoidRef
  -- * Providing/overriding instances
  , VarMut(..)
  , CoerceMut(..)
  , TraverseMut(..)
  , Immutable(..)
  -- * Parts
  , MutPart(..)
  , withPart
  , freezePart, copyPart
  , movePartInto, movePartOver, movePartWithin
  , clonePart, unsafeFreezePart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  -- ** Built-in 'MutPart'
  -- *** Field
  , FieldMut(..), withField, mutField, Label(..)
  -- *** Position
  , PosMut(..), withPos, mutPos
  -- *** Tuple
  , TupleMut(..), withTuple
  -- *** Higher-Kinded Data
  , hkdMutParts, HKDMutParts
  -- *** Other
  , mutFst, mutSnd
  , mutRec
  , coerceRef, withCoerceRef
  -- * Branches
  , MutBranch(..)
  , thawBranch
  , freezeBranch
  , moveBranch
  , copyBranch
  , cloneBranch
  , hasBranch, hasn'tBranch
  , unsafeThawBranch
  , unsafeFreezeBranch
  , withBranch, withBranch_
  , modifyBranch, modifyBranch'
  , updateBranch, updateBranch'
  -- ** Built-in 'MutBranch'
  -- *** Using GHC Generics
  , constrMB, CLabel(..), GMutBranchConstructor, MapRef
  -- *** For common types
  , nilMB, consMB
  , nothingMB, justMB
  , leftMB, rightMB
  -- * Re-exports
  , PrimMonad, PrimState
  ) where

import           Control.Monad.Primitive
import           Data.Mutable.Branches
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Mutable.Parts
import           Data.Primitive.MutVar

