---
title: Instance Wrappers
---

Instance Wrappers
=================

```haskell top hide
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Mutable
import           Data.Primitive.MutVar
import           Data.Vinyl.Functor
import           Data.Vinyl.XRec
import           GHC.Generics
import           Inliterate.Import
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV
```

The following newtype wrappers can imbue types with an automatic `Mutable`
instance, with different behaviors.  They are useful in one of these two
situations:

*One*, if your type comes from an external library, and you still want to use
it as a `Mutable`, these newtype wrappers can be used to treat an external data
type as if it had a `Mutable` instance without actually giving them an orphan
`Mutable` instance.

For example, if an external library offered

```haskell top
newtype VecD = VecD (V.Vector Double)
```

and you don't want to give `VecD` an orphan instance, then you can use
`CoerceMut VecD (V.Vector Double)`, instead.  This has a proper `Mutable`
instance utilizes `MVector`, as it should.

*Two*, if you are leveraging `GRef` to build an automatic mutable version of
your data type but want to override the "default" behavior of a component.

For example, let's say you had a composite type:

```haskell
data MyType = MT
    { mtInt    :: Int
    , mtDoube  :: Double
    , mtString :: String
    }
  deriving Generic

instance Mutable s MyType where
    type Ref s MyType = GRef s MyType
```

This leverages the `Mutable` instances of `Int`, `Double`, and `String`.
However, the normal `Mutable` instance for `String` isn't too great: it uses a
mutable linked list (since it's a type alias for `[Char]`), which is a bit
over-kill.  We can use the `VarMut` newtype wrapper to instead treat `String`
as a single object to be modified whole-wise instead of piecewise:

```haskell top
data MyType = MT
    { mtInt    :: Int
    , mtDoube  :: Double
    , mtString :: VarMut String
    }
  deriving Generic

instance Mutable s MyType where
    type Ref s MyType = GRef s MyType
```

Now `Ref s MyType` is a composite data type of a `MutVar s Int`, a `MutVar s
Double`, and `MutVar s String`.  `VarMut` overrides with a "whole-wise
mutation" instance.

VarMut
------

Overrides with (or provides) "whole-wise" mutation, eliminating any piecewise
granularity.

The type `VarMut String` is a whole-wise mutating reference, and is essentially
`MutVar s String`.  The example above shows a situation where this might be
useful.

CoerceMut
---------

Overrides with (or provides) a mutation in terms of some equivalent type
(usually, a newtype unwrapped version).  Usually useful for providing an
instance for external types.  To repeat the example above:

```haskell
newtype VecD = VecD (V.Vector Double)
```

Then `CoerceMut VecD (V.Vector Double)` has a `Mutable` instance that uses
`MVector` underneath.

TraverseMut
-----------

Overrides with (or provides) a mutation in terms of a type's `Traversable`
instance.  See the information on `TraverseRef` in [the previous
section](/03-automatic-instance-options.html) for more information on the
details for how this instance works.

For example, the `Mutable` instance for `Vector a` is an `MVector s a`, where
each item is included in its "pure" form.  But wouldn't it be nice if we
instead had a mutable `Vector a` instead be `Vector (Ref s a)`, where every
slot contains a mutable value?

You can get that behavior with `TraverseMut Vector a`.

Immutable
---------

This wrapper is typically used to override the mutation of a specific field
when using generic derivation.

For example, looking at the type from above:

```haskell
data MyType = MT
    { mtInt    :: Int
    , mtDoube  :: Double
    , mtString :: String
    }
  deriving Generic
```

Let's say you reaaaalllly don't want that `mtString` field to be mutable.
Like, at all.  You don't want to allocate anything, and you want all copies
into it to be ignored and all freezes to return the original `String`.

In that case, you can use `Immutable`:

```haskell
data MyType = MT
    { mtInt    :: Int
    , mtDoube  :: Double
    , mtString :: Immutable String
    }
  deriving Generic

instance Mutable s MyType where
    type Ref s MyType = GRef s MyType
```

And now `Ref s MyType` will basically be a tupling of `MutVar s Int`, `MutVar s
Double`, and an immutable `String`.  If you try to modify it, modifications
will be ignored.  Freezing a `Ref s MyType` will get the original string back.

This does break a lot of the expectations of mutability, but sometimes this
can be useful for low-level optimizations or hacks.
