
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
  , DefaultMutable
  -- * Instances
  -- ** Generic
  , GRef
  -- ** Higher-Kinded Data Pattern
  , thawHKD, freezeHKD, copyHKD
  -- ** Miscellaneous
  , RecRef(..)
  -- * Parts
  , MutPart(..)
  , freezePart, copyPart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  -- ** Built-in 'MutPart'
  , mutFst, mutSnd
  , fieldMut, Label(..)
  , posMut
  , hkdMutParts
  , mutRec
  ) where

import           Data.Mutable.Class
import           Data.Mutable.MutPart

