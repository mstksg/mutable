{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


-- |
-- Module      : Data.Mutable.MutBranch
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Tools for working with potential branches of piecewise-mutable
-- values.
--
-- If "Data.Mutable.Parts" is for product types, then
-- "Data.Mutable.Branches" is for sum types.
--
-- See <https://mutable.jle.im/06-mutable-branches.html> for an
-- introduction to this module.
module Data.Mutable.Branches (
    MutBranch(..)
  , thawBranch
  , freezeBranch
  , hasBranch, hasn'tBranch
  , moveBranch
  , copyBranch
  , cloneBranch
  , unsafeThawBranch
  , unsafeFreezeBranch
  , withBranch, withBranch_
  , modifyBranch, modifyBranch'
  , updateBranch, updateBranch'
  , modifyBranchM, modifyBranchM'
  , updateBranchM, updateBranchM'
  -- * Built-in 'MutBranch'
  , compMB, idMB
  -- ** Using GHC Generics
  , constrMB, CLabel(..), GMutBranchConstructor, MapRef
  -- ** For common types
  , nilMB, consMB
  , nothingMB, justMB
  , leftMB, rightMB
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Generics.Product.Internal.HList
import           Data.Maybe
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Mutable.Internal
import           Data.Primitive.MutVar
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits
import qualified Data.GenericLens.Internal              as GL
import qualified Data.Generics.Internal.Profunctor.Lens as GLP

-- | A @'MutBranch' s b a@ represents the information that @b@ could
-- potentially be an @a@.  Similar in spirit to a @Prism' b a@.
--
-- @'MutBranch' s b a@ means that @a@ is one potential option that @b@
-- could be in, or that @b@ is a sum type and @a@ is one of the
-- branches/constructors.
--
-- See <https://mutable.jle.im/06-mutable-branches.html> for an
-- introduction to this module.
--
-- If 'Data.Mutable.Parts.MutPart' is for product types, then 'MutBranch'
-- is for sum types.
--
-- In this case, "branch" means "potential option".  For example, the
-- branches of 'Either' are 'Left' and 'Right'.
--
-- The simplest way to make these is by using 'constrMB'.  For instance, to
-- get the two branches of an 'Either':
--
-- @
-- constrMB #_Left   :: MutBranch s (Either a b) a
-- constrMB #_Right  :: MutBranch s (Either a b) b
-- @
--
-- @
-- ghci> r <- 'thawRef' (Left 10)
-- ghci> 'freezeBranch' ('constrMB' #_Left) r
-- Just 10
-- ghci> freezeBranch (constrMB #_Right) r
-- Nothing
-- @
--
-- It uses OverloadedLabels, but requires an underscore before the
-- constructor name due to limitations in the extension.
--
-- One nice way to /use/ these is with 'withBranch_':
--
-- @
-- ghci> r <- 'thawRef' (Just 10)
-- ghci> 'withBranch_' (constrMB #_Just) $ \i ->    -- @i@ is an Int ref
--    ..   modifyRef i (+ 1)
-- ghci> 'freezeRef' r
-- Just 11
-- @
--
-- @
-- ghci> r <- thawRef Nothing
-- ghci> withBranch_ (constrMB #_Just) $ \i ->    -- @i@ is an Int ref
--    ..   modifyRef i (+ 1)
-- ghci> freezeRef r
-- Nothing
-- @
--
-- Perhaps the most useful usage of this abstraction is for recursive data
-- types.
--
-- @
-- data List a = Nil | Cons a (List a)
--   deriving Generic
--
-- instance Mutable s a => 'Mutable' s (List a) where
--     type Ref s (List a) = 'GRef' s (List a)
-- @
--
-- @'GRef' s (List a)@ is now a mutable linked list!  Once we make the
-- 'MutBranch' for the nil and cons cases:
--
-- @
-- nilBranch :: MutBranch s (List a) ()
-- nilBranch = constrMB #_Nil
--
-- consBranch :: MutBranch s (List a) (a, List a)
-- consBranch = constrMB #_Cons
-- @
--
--
-- Here is a function to check if a linked list is currently empty:
--
-- @
-- isEmpty
--     :: (PrimMonad m, Mutable s a)
--     => Ref s (List a)
--     -> m Bool
-- isEmpty = hasBranch nilBranch
-- @
--
-- Here is one to "pop" a mutable linked list, giving us the first value
-- and shifting the rest of the list up.
--
-- @
-- popStack
--     :: (PrimMonad m, Mutable s a)
--     => Ref s (List a)
--     -> m (Maybe a)
-- popStack r = do
--     c <- projectBranch consBranch r
--     case c of
--       Nothing      -> pure Nothing
--       Just (x, xs) -> do
--         moveRef r xs
--         Just <$> freezeRef x
-- @
--
-- And here is a function to concatenate a second linked list to the end of a
-- first one.
--
-- @
-- concatLists
--     :: (PrimMonad m, Mutable s a)
--     => Ref s (List a)
--     -> Ref s (List a)
--     -> m ()
-- concatLists l1 l2 = do
--     c <- projectBranch consBranch l1
--     case c of
--       Nothing      -> moveRef l1 l2
--       Just (_, xs) -> concatLists xs l2
-- @
data MutBranch s b a = MutBranch
    { -- | With a 'MutBranch', attempt to get the mutable contents of
      -- a branch of a mutable
      -- @s@, if possible.
      --
      -- @
      -- ghci> r <- thawRef (Left 10)
      -- ghci> s <- projectBranch (constrMB #_Left) r
      -- ghci> case s of Just s' -> freezeRef s'
      -- 10
      -- @
      --
      -- @
      -- ghci> r <- thawRef (Right True)
      -- ghci> s <- projectBranch (constrMB #_Left) r
      -- ghci> case s of Nothing -> "it was Right"
      -- "it was Right"
      -- @
      projectBranch :: forall m. (PrimMonad m, PrimState m ~ s) => Ref s b -> m (Maybe (Ref s a))
      -- | Embed an @a@ ref as a part of a larger @s@ ref.  Note that this
      -- /does not copy or clone/: any mutations to the @a@ ref will be
      -- reflected in the @s@ ref, as long as the @s@ ref maintains the
      -- reference.
      --
      -- @
      -- ghci> r <- thawRef 100
      -- ghci> s <- embedBranch (constMB #_Left) r
      -- ghci> freezeRef s
      -- Left 100
      -- ghci> modifyRef r (+ 1)
      -- ghci> freezeRef s
      -- Left 101
      -- @
      --
      -- Any mutations on @s@ (as long as they keep the same branch) will
      -- also affect @a@:
      --
      -- @
      -- ghci> copyRef s (Left 0)
      -- ghci> freezeRef r
      -- 0
      -- @
      --
      -- However, "switching branches" on an 'Either' ref will cause it to
      -- loose the original reference:
      --
      -- @
      -- ghci> copyRef s (Right True)
      -- ghci> copyRef s (Left 999)
      -- ghci> freezeRef r
      -- 0
      -- @
    , embedBranch :: forall m. (PrimMonad m, PrimState m ~ s) => Ref s a -> m (Ref s b)
    }

-- | Compose two 'MutBranch's, to drill down on what is being focused.
compMB :: MutBranch s a b -> MutBranch s b c -> MutBranch s a c
compMB mb1 mb2 = MutBranch
    { projectBranch = projectBranch mb1 >=> \case
        Nothing -> pure Nothing
        Just s  -> projectBranch mb2 s
    , embedBranch = embedBranch mb1 <=< embedBranch mb2
    }

-- | An identity 'MutBranch', treating the item itself as a whole branch.
-- 'cloneBranch' will always "match".
idMB :: MutBranch s a a
idMB = MutBranch (pure . Just) pure

-- | With a 'MutBranch', thaw an @a@ into a mutable @s@ on that branch.
--
-- @
-- ghci> r <- 'thawBranch' ('constrMB' #_Left) 10
-- ghci> 'freezeRef' r
-- Left 10
-- @
thawBranch
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a
    -> a
    -> m (Ref s b)
thawBranch mb = embedBranch mb <=< thawRef

-- | With a 'MutBranch', read out a specific @a@ branch of an @s@, if it exists.
--
-- @
-- ghci> r <- 'thawRef' (Left 10)
-- ghci> 'freezeBranch' ('constrMB' #_Left) r
-- Just 10
-- ghci> freezeBranch (constrMB #_Right) r
-- Nothing
-- @
freezeBranch
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a    -- ^ How to check if is @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of
    -> m (Maybe a)
freezeBranch mb = mapM freezeRef <=< projectBranch mb

-- | Check if an @s@ is currently a certain branch @a@.
hasBranch
    :: (PrimMonad m, PrimState m ~ s)
    => MutBranch s b a
    -> Ref s b
    -> m Bool
hasBranch mb = fmap isJust . projectBranch mb

-- | Check if an @s@ is /not/ currently a certain branch @a@.
hasn'tBranch
    :: (PrimMonad m, PrimState m ~ s)
    => MutBranch s b a
    -> Ref s b
    -> m Bool
hasn'tBranch mb = fmap isNothing . projectBranch mb

-- | With a 'MutBranch', /set/ @s@ to have the branch @a@.
--
-- @
-- ghci> r <- 'thawRef' (Left 10)
-- ghci> 'copyBranch' ('constrMB' #_Left) r 5678
-- ghci> 'freezeRef' r
-- Left 5678
-- ghci> copyBranch (constrMB #_Right) r True
-- ghci> freezeRef r
-- Right True
-- @
copyBranch
    :: (Mutable s b, Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b              -- ^ Structure to write into
    -> a                    -- ^ Value to set @s@ to be
    -> m ()
copyBranch mb r = moveBranch mb r <=< thawRef

-- | With a 'MutBranch', overwrite an @s@ as an @a@, on that branch.
--
-- @
-- ghci> r <- thawRef (Left 10)
-- ghci> s <- thawRef 100
-- ghci> moveBranch (constrMB #_Left) r s
-- ghci> freezeRef r
-- Left 100
-- ghci> t <- thawRef True
-- ghci> moveBranch (constrMB #_Right) r t
-- ghci> freezeRef r
-- Right True
-- @
moveBranch
    :: (Mutable s b, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a
    -> Ref s b
    -> Ref s a
    -> m ()
moveBranch mb r = moveRef r <=< embedBranch mb

-- | With a 'MutBranch', attempt to clone out a branch of a mutable
-- @s@, if possible.
--
-- @
-- ghci> r <- thawRef (Left 10)
-- ghci> s <- cloneBranch (constrMB #_Left)
-- ghci> case s of Just s' -> freezeRef s'
-- 10
-- @
--
-- @
-- ghci> r <- thawRef (Right True)
-- ghci> s <- cloneBranch (constrMB #_Left)
-- ghci> case s of Nothing -> "it was Right"
-- "it was Right"
-- @
cloneBranch
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b              -- ^ Structure to read out of
    -> m (Maybe (Ref s a))
cloneBranch mb = mapM cloneRef <=< projectBranch mb

-- | A non-copying version of 'freezeBranch' that can be more efficient
-- for types where the mutable representation is the same as the immutable
-- one (like 'V.Vector').
--
-- This is safe as long as you never again modify the mutable
-- reference, since it can potentially directly mutate the frozen value
-- magically.
unsafeFreezeBranch
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a    -- ^ How to check if is @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of
    -> m (Maybe a)
unsafeFreezeBranch mb = mapM unsafeFreezeRef <=< projectBranch mb

-- | A non-copying version of 'thawBranch' that can be more efficient for
-- types where the mutable representation is the same as the immutable one
-- (like 'V.Vector').
--
-- This is safe as long as you never again use the original pure value,
-- since it can potentially directly mutate it.
unsafeThawBranch
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a
    -> a
    -> m (Ref s b)
unsafeThawBranch mb = embedBranch mb <=< unsafeThawRef


-- | With a 'MutBranch', if an @s@ is on the @a@ branch, perform an action
-- on the @a@ reference and overwrite the @s@ with the modified @a@.
-- Returns the result of the action, if @a@ was found.
--
-- @
-- ghci> r <- 'thawRef' (Just 10)
-- ghci> 'withBranch_' ('constrMB' #_Just) $ \i ->    -- @i@ is an Int ref
--    ..   'modifyRef' i (+ 1)
-- ghci> 'freezeRef' r
-- Just 11
-- @
--
-- @
-- ghci> r <- thawRef Nothing
-- ghci> withBranch_ (constrMB #_Just) $ \i ->    -- @i@ is an Int ref
--    ..   modifyRef i (+ 1)
-- ghci> freezeRef r
-- Nothing
-- @
withBranch
    :: (PrimMonad m, PrimState m ~ s)
    => MutBranch s b a    -- ^ How to check if is @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (Ref s a -> m r)   -- ^ Action to perform on the @a@ branch of @s@
    -> m (Maybe r)
withBranch mb r f = mapM f =<< projectBranch mb r

-- | 'withBranch', but discarding the returned value.
withBranch_
    :: (PrimMonad m, PrimState m ~ s)
    => MutBranch s b a    -- ^ How to check if is @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (Ref s a -> m r)   -- ^ Action to perform on the @a@ branch of @s@
    -> m ()
withBranch_ mb r = void . withBranch mb r

-- | With a 'MutBranch', run a pure function over a potential branch @a@ of
-- @s@.  If @s@ is not on that branch, leaves @s@ unchanged.
--
-- @
-- ghci> r <- 'thawRef' (Just 10)
-- ghci> 'modifyBranch' ('constrMB' #_Just) r (+ 1)
-- ghci> freezeRef r
-- Just 11
-- @
--
-- @
-- ghci> r <- thawRef Nothing
-- ghci> modifyBranch (constrMB #_Just) r (+ 1)
-- ghci> freezeRef r
-- Nothing
-- @
modifyBranch
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> a)             -- ^ Pure function modifying @a@
    -> m ()
modifyBranch mb r f = withBranch_ mb r (`modifyRef` f)

-- | 'modifyBranch', but forces the result before storing it back in the
-- reference.
modifyBranch'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> a)             -- ^ Pure function modifying @a@
    -> m ()
modifyBranch' mb r f = withBranch_ mb r (`modifyRef'` f)

-- | 'modifyBranch' but for a monadic function.  Uses 'copyRef' into the
-- reference after the action is completed.
modifyBranchM
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> m a)             -- ^ Monadic function modifying @a@
    -> m ()
modifyBranchM mb r f = withBranch_ mb r (`modifyRefM` f)

-- | 'modifyBranchM', but forces the result before storing it back in the
-- reference.
modifyBranchM'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> m a)             -- ^ Monadic function modifying @a@
    -> m ()
modifyBranchM' mb r f = withBranch_ mb r (`modifyRefM'` f)

-- | With a 'MutBranch', run a pure function over a potential branch @a@ of
-- @s@.  The function returns the updated @a@ and also an output value to
-- observe.  If @s@ is not on that branch, leaves @s@ unchanged.
--
-- @
-- ghci> r <- 'thawRef' (Just 10)
-- ghci> 'updateBranch' ('constrMB' #_Just) r $ \i -> (i + 1, show i)
-- Just "10"
-- ghci> 'freezeRef' r
-- Just 11
-- @
--
-- @
-- ghci> r <- thawRef Nothing
-- ghci> updateBranch (constrMB #_Just) r $ \i -> (i + 1, show i)
-- Nothing
-- ghci> freezeRef r
-- Nothing
-- @
updateBranch
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> (a, r))
    -> m (Maybe r)
updateBranch mb r f = withBranch mb r (`updateRef` f)

-- | 'updateBranch', but forces the result before storing it back in the
-- reference.
updateBranch'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> (a, r))
    -> m (Maybe r)
updateBranch' mb r f = withBranch mb r (`updateRef'` f)

-- | 'updateBranch' but for a monadic function.  Uses 'copyRef' into the
-- reference after the action is completed.
updateBranchM
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> m (a, r))
    -> m (Maybe r)
updateBranchM mb r f = withBranch mb r (`updateRefM` f)

-- | 'updateBranchM', but forces the result before storing it back in the
-- reference.
updateBranchM'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutBranch s b a      -- ^ How to check if @s@ is an @a@
    -> Ref s b            -- ^ Structure to read out of and write into
    -> (a -> m (a, r))
    -> m (Maybe r)
updateBranchM' mb r f = withBranch mb r (`updateRefM'` f)



-- | A version of 'Data.Vinyl.Derived.Label' that removes an underscore at
-- the beginning when used with -XOverloadedLabels.  Used to specify
-- constructors, since labels are currently not able to start with capital
-- letters.
data CLabel (ctor :: Symbol) = CLabel

instance (ctor_ ~ AppendSymbol "_" ctor) => IsLabel ctor_ (CLabel ctor) where
    fromLabel = CLabel



-- | Typeclass powering 'constrMB' using GHC Generics.
--
-- Heavily inspired by "Data.Generics.Sum.Constructors".
class (GMutable s f, Mutable s a) => GMutBranchConstructor (ctor :: Symbol) s f a | ctor f -> a where
    gmbcProj  :: (PrimMonad m, PrimState m ~ s) => CLabel ctor -> GRef_ s f x -> m (Maybe (Ref s a))
    gmbcEmbed :: (PrimMonad m, PrimState m ~ s) => CLabel ctor -> Ref s a -> m (GRef_ s f x)

instance
      ( GMutable s f
      , Mutable s a
      , GIsList (GRef_ s f) (GRef_ s f) (MapRef s as) (MapRef s as)
      , GIsList f f as as
      , ListTuple a a as as
      , ListRefTuple s b as
      , Ref s a ~ b
      )
      => GMutBranchConstructor ctor s (M1 C ('MetaCons ctor fixity fields) f) a where
    gmbcProj _  = pure . Just
                . GLP.view (glist . tupledRef @s @b @as)
                . unM1
    gmbcEmbed _ = pure
                . M1
                . GLP.view (GL.fromIso (glist . tupledRef @s @b @as))


instance GMutBranchConstructor ctor m f a => GMutBranchConstructor ctor m (M1 D meta f) a where
    gmbcProj  lb = gmbcProj lb . unM1
    gmbcEmbed lb = fmap M1 . gmbcEmbed lb

instance
      ( Mutable s a
      , GMutBranchSum ctor (GL.HasCtorP ctor l) s l r a
      )
      => GMutBranchConstructor ctor s (l :+: r) a where
    gmbcProj  = gmbsProj @ctor @(GL.HasCtorP ctor l)
    gmbcEmbed = gmbsEmbed @ctor @(GL.HasCtorP ctor l)


class ( GMutable s l
      , GMutable s r
      , Mutable s a
      ) => GMutBranchSum (ctor :: Symbol) (contains :: Bool) s l r a | ctor l r -> a where
    gmbsProj
        :: (PrimMonad m, PrimState m ~ s)
        => CLabel ctor
        -> MutSumF s (GRef_ s l) (GRef_ s r) x
        -> m (Maybe (Ref s a))
    gmbsEmbed
        :: (PrimMonad m, PrimState m ~ s)
        => CLabel ctor
        -> Ref s a
        -> m (MutSumF s (GRef_ s l) (GRef_ s r) x)

instance
      ( GMutable s r
      , GMutBranchConstructor ctor s l a
      , GIsList (GRef_ s l) (GRef_ s l) (MapRef s as) (MapRef s as)
      , GIsList l l as as
      , ListTuple a a as as
      , ListRefTuple s b as
      , Ref s a ~ b
      )
      => GMutBranchSum ctor 'True s l r a where
    gmbsProj lb (MutSumF r) = readMutVar r >>= \case
      L1 x -> gmbcProj lb x
      R1 _ -> pure Nothing
    gmbsEmbed _ = fmap MutSumF . newMutVar . L1
                . GLP.view (GL.fromIso (glist . tupledRef @s @b @as))

instance
      ( GMutable s l
      , GMutBranchConstructor ctor s r a
      , Ref s a ~ b
      )
      => GMutBranchSum ctor 'False s l r a where
    gmbsProj lb (MutSumF r) = readMutVar r >>= \case
      L1 _ -> pure Nothing
      R1 x -> gmbcProj lb x
    gmbsEmbed lb r = do
      gr <- gmbcEmbed lb r
      MutSumF <$> newMutVar (R1 gr)


-- | Create a 'MutBranch' for any data type with a 'Generic' instance by
-- specifying the constructor name using OverloadedLabels
--
-- @
-- ghci> r <- 'thawRef' (Left 10)
-- ghci> 'freezeBranch' ('constrMB' #_Left) r
-- Just 10
-- ghci> freezeBranch (constrMB #_Right) r
-- Nothing
-- @
--
-- Note that due to limitations in OverloadedLabels, you must prefix the
-- constructor name with an undescore.
--
-- There also isn't currently any way to utilize OverloadedLabels with
-- operator identifiers, so using it with operator constructors (like @:@
-- and @[]@) requires explicit TypeApplications:
--
-- @
-- -- | 'MutBranch' focusing on the cons case of a list
-- consMB :: (PrimMonad m, Mutable s a) => MutBranch s [a] (a, [a])
-- consMB = 'constrMB' ('CLabel' @":")
-- @
constrMB
    :: forall ctor s b a.
     ( Ref s b ~ GRef s b
     , GMutBranchConstructor ctor s (Rep b) a
     )
    => CLabel ctor
    -> MutBranch s b a
constrMB l = MutBranch
    { projectBranch = gmbcProj l . unGRef
    , embedBranch   = fmap GRef . gmbcEmbed l
    }

-- | 'MutBranch' focusing on the nil case of a list
nilMB :: Mutable s a => MutBranch s [a] ()
nilMB = constrMB (CLabel @"[]")

-- | 'MutBranch' focusing on the cons case of a list
consMB :: Mutable s a => MutBranch s [a] (a, [a])
consMB = constrMB (CLabel @":")

-- | 'MutBranch' focusing on the 'Nothing' case of a 'Maybe'
nothingMB :: Mutable s a => MutBranch s (Maybe a) ()
nothingMB = constrMB #_Nothing

-- | 'MutBranch' focusing on the 'Just' case of a 'Maybe'
justMB :: Mutable s a => MutBranch s (Maybe a) a
justMB = constrMB #_Just

-- | 'MutBranch' focusing on the 'Left' case of an 'Either'
leftMB :: (Mutable s a, Mutable s b) => MutBranch s (Either a b) a
leftMB = constrMB #_Left

-- | 'MutBranch' focusing on the 'Right' case of an 'Either'
rightMB :: (Mutable s a, Mutable s b) => MutBranch s (Either a b) b
rightMB = constrMB #_Right
