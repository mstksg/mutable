---
title: Mutable Branches
---

Mutable Branches
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

The `MutPart` abstraction lets us "zoom into" a smaller part of a
piecewise-mutable product type.  But what about sum types?

Let's look at *[Data.Mutable.Branches][DMB]* to tackle this.

[DMB]: https://hackage.haskell.org/package/mutable/docs/Data-Mutable-Branches.html

Conceptually, mutable sum types are a completely different beast.  A mutable
product type is just a (pure) tuple of its mutable fields.

A mutable *sum type* is actually a mutable reference to *one of its possible
branches*.

For example, let's consider a simple sum type:

```haskell top
data IntOrBool = IBInt  Int
               | IBBool Bool
```

Its mutable version would be:

```haskell
newtype IntOrBoolRef s = IBR
    { getIBR :: MutVar s (Either (MutVar s Int) (MutVar s Bool))
    }
```

It's a mutable reference to *either* a mutable `Int` or a mutable `Bool`.

This is probably *most useful* when thinking about recursive types, like lists:

```haskell top
data List a = Nil | Cons a (List a)
  deriving (Show, Generic)

instance (Mutable m a, PrimMonad m) => Mutable m (List a) where
    type Ref m (List a) = GRef m (List a)
```

The `GRef m (List a)` is now a *[mutable linked list][ll]* --- the good old
fashioned data type that people learn to implement in Java or C++ or what have
you.  It's a reference to a cell that is either a `Nil` cell or a `Cons` cell
with a reference to an `a` and a reference to another list cell.

[ll]: https://en.wikipedia.org/wiki/Linked_list

The main tool to work with mutable branches is to use the `MutBranch` data
type, from *[Data.Mutable.Branches][DMB]*, which specifies which "branch" on mutable
sum type to work with.  In the case of `IntOrBool`, it we might have a
`MutBranch m IntOrBool Int` for the `Int` case and a `MutBranch m IntOrBool
Bool` for the `Bool` case.  In the case of `List`, since it has a `Generic`
instance, we can use `constrMB` to create a `MutBranch` based on the
constructor name.

```haskell top
nilBranch
    :: (PrimMonad m, Mutable m a)
    => MutBranch m (List a) ()
nilBranch = constrMB #_Nil

consBranch
    :: (PrimMonad m, Mutable m a)
    => MutBranch m (List a) (a, List a)
consBranch = constrMB #_Cons
```

`nilBranch` represents the `Nil` constructor (containing nothing, `()`), and
`consBranch` represents the `Cons` constructor (containing a mutable `(a, List
a)`).

Note that due to limitations in OverloadedLabels, we're required to add that
underscore before the constructor name.  (If your constructor is an operator,
you'd have to do something like `constrMB (CLabel @":")`).

The simplest way to use a `MutBranch` is to check if a reference is currently
on that branch, with `hasBranch`:

```haskell top
-- | Check if a mutable linked list is currently empty
isEmpty
    :: (PrimMonad m, Mutable m a)
    => Ref m (List a)
    -> m Bool
isEmpty = hasBranch nilBranch
```

Using the API of *[Data.Mutable.Branches][DMB]*, we can write a function to "pop" a
mutable linked list, giving us the first value and shifting the rest of the
list up.


```haskell top
popStack
    :: (PrimMonad m, Mutable m a)
    => Ref m (List a)
    -> m (Maybe a)
popStack xs = do
    c <- projectBranch consBranch xs
    forM c $ \(y, ys) -> do
      o <- freezeRef y
      moveRef xs ys
      pure o
```

And here is a function to concatenate a second linked list to the end of a
first one.

```haskell top
concatLists
    :: (PrimMonad m, Mutable m a)
    => Ref m (List a)
    -> Ref m (List a)
    -> m ()
concatLists l1 l2 = do
    c <- projectBranch consBranch l1
    case c of
      Nothing      -> moveRef l1 l2
      Just (_, xs) -> concatLists xs l2
```

The main benefit of using this library --- with `GRef` and `MutBranch`, are a
set of automatically generated type-safe tools for dealing with mutable
options.

