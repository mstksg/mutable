---
title: Getting Started
---

Getting Started
===============

```haskell top hide
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.Mutable
import           Data.Primitive.MutVar
import           GHC.Generics
import           Inliterate.Import
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

instance Show a => AskInliterate (V.Vector a)
```

If you have a data type like:

```haskell top
data MyType = MT
    { mtInt    :: Int
    , mtDouble :: Double
    , mtVec    :: V.Vector Double
    }
  deriving (Show, Generic)
```

Then you can give it an automatically derived mutable version:

```haskell top
instance PrimMonad m => Mutable m MyType where
    type Ref m MyType = GRef m MyType
```

`Ref m MyType` is now a "mutable `MyType`", like how `MVector s a` is a
"mutable `Vector a`".

We now have some nice operations:

Wholewise operations
--------------------

Sometimes you just want to operate on the whole `MyType`.  Well, you now have:

```haskell
-- | Allocate a mutable 'MyType' in the monad m
thawRef :: MyType -> Ref m MyType

-- | "Freeze" a mutable 'MyType'
freezeRef :: Ref m MyType -> m MyType

-- | Overwrite a mutable 'MyType' with the contents of a pure one.
copyRef :: Ref m MyType -> MyType -> m ()

-- | Run an updating function on a whole 'MyType'
modifyRef :: Ref m MyType -> (MyType -> MyType) -> m ()
```

This is nice, but we really the juicy stuff: a way to modify each part
individually.  For that, we have two main mechanisms: the field name based
ones, and the position based ones.
