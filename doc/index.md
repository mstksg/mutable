---
title: Home
---

Mutability can be Awesome
=========================

```haskell top hide
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           GHC.Generics
import           Inliterate.Import
import           Data.Mutable
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

instance Show a => AskInliterate (V.Vector a)
```

Take back the power of **mutable objects** with 

Bring back the power of mutable data types with type-safety and explicit
mutability.  Associate and generate "piecewise-mutable" versions for your
composite data types in a composable and automatic way.  Think of it like a
"generalized `MVector` for all ADTs".

```haskell top
data MyType = MT
    { mtInt    :: Int
    , mtDouble :: Double
    , mtVec    :: V.Vector Double
    }
  deriving (Show, Generic)

instance PrimMonad m => Mutable m MyType where
    type Ref m MyType = GRef m MyType
```

```haskell top hide
instance AskInliterate MyType
```

The type `Ref m MyType` is now a mutable version of `MyType`:

```haskell top
doStuff :: MyType -> MyType
doStuff x = runST $ do
    r <- thawRef x

    replicateM_ 1000 $ do

        -- modify the Int in `mtInt`
        modifyPart (fieldMut #mtInt) r (+ 1)

        -- the `mtVec` field is now an MVector
        withField #mtVec r $ \v ->
          MV.modify v (+1) 0

    freezeRef r
```

```haskell eval
doStuff $ MT 0 19.3 (V.fromList [1..12])
```

If you were to do this normally with pure values, this would be extremely
expensive, especially if `mtVec` is a huge vector --- it would require copying
every item in the entire vector every step, being *O(n \* l)* , with *n* number
of repetitions and *l* length of vector and number of fields.  With mutable
vectors and mutable cells, this now becomes *O(n + l)*.
