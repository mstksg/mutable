---
title: Home
---

Mutability can be Awesome
=========================

[![mutable on Hackage](https://img.shields.io/hackage/v/mutable.svg?maxAge=86400)](https://hackage.haskell.org/package/mutable)
[![Build Status](https://travis-ci.org/mstksg/mutable.svg?branch=master)](https://travis-ci.org/mstksg/mutable)

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
```

Take back the power of **mutable objects** with all the **safety** and explicit
state of Haskell. Associate and generate "piecewise-mutable" versions for your
composite data types in a composable and automatic way.  Think of it like a
"generalized `MVector` for all ADTs".  It also leverages GHC Generics to make
working with piecewise mutability as simple as possible.

Making piecewise updates on your giant composite data types (like artificial
neural networks or game states in your game loop) got you down?  Tired of
requiring a full deep copy every time you make a small change, and want to be
able to build mutable versions of your types automatically in composable ways?
This is the package for you.

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

The type `Ref m MyType` is now a "mutable `MyType`", just like how `MVector s
a` is a "mutable `Vector a`".  You have:

```haskell
thawRef   :: MyType -> m (Ref m MyType)
freezeRef :: Ref m MyType -> m MyType
```

You can use `thawRef` to allocate a mutable `MyType` that essentially consists
of a mutable `Int`, a mutable `Double`, and a mutable `Vector` (an `MVector`)
all tupled together. You can edit these pieces in isolation, and then
`freezeRef` it all back together:

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

The main motivation for this library is to implement *automatically derivable*
piecewise-mutable references for the purposes of mutation-heavy algorithms,
like artificial neural networks.  In the end, you're able to have an Artificial
Neural Network (which can have huuuuge vectors) and being able to do piecewise
updates on them (automatically) without having to copy over the entire network
every training step.

Check out **[the getting started page](/01-getting-started.html)**, or the **[Haddock
Documentation][docs]** to jump right in!

[docs]: https://hackage.haskell.org/package/mutable
