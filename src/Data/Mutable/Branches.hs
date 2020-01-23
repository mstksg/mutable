{-# LANGUAGE AllowAmbiguousTypes    #-}
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
import           Data.Maybe
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Primitive.MutVar
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits
import qualified Data.GenericLens.Internal              as GL
import qualified Data.Generics.Internal.Profunctor.Lens as GLP

-- | A @'MutBranch' m s a@ represents the information that @s@ could
-- potentially be an @a@.  Similar in spirit to a @Prism' s a@.
--
-- @'MutBranch' m s a@ means that @a@ is one potential option that @s@
-- could be in, or that @s@ is a sum type and @a@ is one of the
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
-- constrMB #_Left   :: MutBranch m (Either a b) a
-- constrMB #_Right  :: MutBranch m (Either a b) b
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
-- instance Mutable m a => 'Mutable' m (List a) where
--     type Ref m (List a) = 'GRef' m (List a)
-- @
--
-- @'GRef' m (List a)@ is now a mutable linked list!  Once we make the
-- 'MutBranch' for the nil and cons cases:
--
-- @
-- nilBranch :: MutBranch m (List a) ()
-- nilBranch = constrMB #_Nil
-- 
-- consBranch :: MutBranch m (List a) (a, List a)
-- consBranch = constrMB #_Cons
-- @
--
--
-- Here is a function to check if a linked list is currently empty:
--
-- @
-- isEmpty
--     :: (PrimMonad m, Mutable m a)
--     => Ref m (List a)
--     -> m Bool
-- isEmpty = hasBranch nilBranch
-- @
--
-- Here is one to "pop" a mutable linked list, giving us the first value
-- and shifting the rest of the list up.
--
-- @
-- popStack
--     :: (PrimMonad m, Mutable m a)
--     => Ref m (List a)
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
--     :: (PrimMonad m, Mutable m a)
--     => Ref m (List a)
--     -> Ref m (List a)
--     -> m ()
-- concatLists l1 l2 = do
--     c <- projectBranch consBranch l1
--     case c of
--       Nothing      -> moveRef l1 l2
--       Just (_, xs) -> concatLists xs l2
-- @
data MutBranch m s a = MutBranch
    { -- | With a 'MutBranch', attempt to get the mutable contents of
      -- a branch of a mutable
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
      projectBranch :: Ref m s -> m (Maybe (Ref m a))
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
    , embedBranch :: Ref m a -> m (Ref m s)
    }

-- | Compose two 'MutBranch's, to drill down on what is being focused.
compMB :: Monad m => MutBranch m a b -> MutBranch m b c -> MutBranch m a c
compMB mb1 mb2 = MutBranch
    { projectBranch = projectBranch mb1 >=> \case
        Nothing -> pure Nothing
        Just s  -> projectBranch mb2 s
    , embedBranch = embedBranch mb1 <=< embedBranch mb2
    }

-- | An identity 'MutBranch', treating the item itself as a whole branch.
-- 'cloneBranch' will always "match".
idMB :: Applicative m => MutBranch m a a
idMB = MutBranch (pure . Just) pure

-- | With a 'MutBranch', thaw an @a@ into a mutable @s@ on that branch.
--
-- @
-- ghci> r <- 'thawBranch' ('constrMB' #_Left) 10
-- ghci> 'freezeRef' r
-- Left 10
-- @
thawBranch
    :: Mutable m a
    => MutBranch m s a
    -> a
    -> m (Ref m s)
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
    :: Mutable m a
    => MutBranch m s a    -- ^ How to check if is @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of
    -> m (Maybe a)
freezeBranch mb = mapM freezeRef <=< projectBranch mb

-- | Check if an @s@ is currently a certain branch @a@.
hasBranch
    :: Mutable m a
    => MutBranch m s a
    -> Ref m s
    -> m Bool
hasBranch mb = fmap isJust . projectBranch mb

-- | Check if an @s@ is /not/ currently a certain branch @a@.
hasn'tBranch
    :: Mutable m a
    => MutBranch m s a
    -> Ref m s
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
    :: (Mutable m s, Mutable m a)
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s              -- ^ Structure to write into
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
    :: Mutable m s
    => MutBranch m s a
    -> Ref m s
    -> Ref m a
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
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s              -- ^ Structure to read out of
    -> m (Maybe (Ref m a))
cloneBranch mb = mapM cloneRef <=< projectBranch mb

-- | A non-copying version of 'freezeBranch' that can be more efficient
-- for types where the mutable representation is the same as the immutable
-- one (like 'V.Vector').
--
-- This is safe as long as you never again modify the mutable
-- reference, since it can potentially directly mutate the frozen value
-- magically.
unsafeFreezeBranch
    :: Mutable m a
    => MutBranch m s a    -- ^ How to check if is @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of
    -> m (Maybe a)
unsafeFreezeBranch mb = mapM unsafeFreezeRef <=< projectBranch mb

-- | A non-copying version of 'thawBranch' that can be more efficient for
-- types where the mutable representation is the same as the immutable one
-- (like 'V.Vector').
--
-- This is safe as long as you never again use the original pure value,
-- since it can potentially directly mutate it.
unsafeThawBranch
    :: Mutable m a
    => MutBranch m s a
    -> a
    -> m (Ref m s)
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
    :: Mutable m a
    => MutBranch m s a    -- ^ How to check if is @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (Ref m a -> m b)   -- ^ Action to perform on the @a@ branch of @s@
    -> m (Maybe b)
withBranch mb r f = mapM f =<< projectBranch mb r

-- | 'withBranch', but discarding the returned value.
withBranch_
    :: Mutable m a
    => MutBranch m s a    -- ^ How to check if is @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (Ref m a -> m b)   -- ^ Action to perform on the @a@ branch of @s@
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
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (a -> a)             -- ^ Pure function modifying @a@
    -> m ()
modifyBranch mb r f = withBranch_ mb r (`modifyRef` f)

-- | 'modifyBranch', but forces the result before storing it back in the
-- reference.
modifyBranch'
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (a -> a)             -- ^ Pure function modifying @a@
    -> m ()
modifyBranch' mb r f = withBranch_ mb r (`modifyRef'` f)

-- | 'modifyBranch' but for a monadic function.  Uses 'copyRef' into the
-- reference after the action is completed.
modifyBranchM
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (a -> m a)             -- ^ Monadic function modifying @a@
    -> m ()
modifyBranchM mb r f = withBranch_ mb r (`modifyRefM` f)

-- | 'modifyBranchM', but forces the result before storing it back in the
-- reference.
modifyBranchM'
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
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
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (a -> (a, b))
    -> m (Maybe b)
updateBranch mb r f = withBranch mb r (`updateRef` f)

-- | 'updateBranch', but forces the result before storing it back in the
-- reference.
updateBranch'
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (a -> (a, b))
    -> m (Maybe b)
updateBranch' mb r f = withBranch mb r (`updateRef'` f)

-- | 'updateBranch' but for a monadic function.  Uses 'copyRef' into the
-- reference after the action is completed.
updateBranchM
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (a -> m (a, b))
    -> m (Maybe b)
updateBranchM mb r f = withBranch mb r (`updateRefM` f)

-- | 'updateBranchM', but forces the result before storing it back in the
-- reference.
updateBranchM'
    :: Mutable m a
    => MutBranch m s a      -- ^ How to check if @s@ is an @a@
    -> Ref m s            -- ^ Structure to read out of and write into
    -> (a -> m (a, b))
    -> m (Maybe b)
updateBranchM' mb r f = withBranch mb r (`updateRefM'` f)



-- | A version of 'Data.Vinyl.Derived.Label' that removes an underscore at
-- the beginning when used with -XOverloadedLabels.  Used to specify
-- constructors, since labels are currently not able to start with capital
-- letters.
data CLabel (ctor :: Symbol) = CLabel

instance (ctor_ ~ AppendSymbol "_" ctor) => IsLabel ctor_ (CLabel ctor) where
    fromLabel = CLabel



-- | Typeclass powering 'constrMB' using GHC Generics.
class (GMutable m f, Mutable m a) => GMutBranchConstructor (ctor :: Symbol) m f a | ctor f -> a where
    gmbcProj  :: CLabel ctor -> GRef_ m f x -> m (Maybe (Ref m a))
    gmbcEmbed :: CLabel ctor -> Ref m a -> m (GRef_ m f x)

instance
      ( GMutable m f
      , Mutable m a
      , GL.GIsList (GRef_ m f) (GRef_ m f) (MapRef m as) (MapRef m as)
      , GL.GIsList f f as as
      , GL.ListTuple a as
      , GL.ListTuple b (MapRef m as)
      , Ref m a ~ b
      )
      => GMutBranchConstructor ctor m (M1 C ('MetaCons ctor fixity fields) f) a where
    gmbcProj _  = pure . Just . GL.listToTuple . GLP.view GL.glist . unM1
    gmbcEmbed _ = pure . M1 . GLP.view GL.glistR . GL.tupleToList

instance GMutBranchConstructor ctor m f a => GMutBranchConstructor ctor m (M1 D meta f) a where
    gmbcProj  lb = gmbcProj lb . unM1
    gmbcEmbed lb = fmap M1 . gmbcEmbed lb

instance
      ( PrimMonad m
      , Mutable m a
      , GMutBranchSum ctor (GL.HasCtorP ctor l) m l r a
      )
      => GMutBranchConstructor ctor m (l :+: r) a where
    gmbcProj  = gmbsProj @ctor @(GL.HasCtorP ctor l)
    gmbcEmbed = gmbsEmbed @ctor @(GL.HasCtorP ctor l)

class (GMutable m l, GMutable m r, Mutable m a) => GMutBranchSum (ctor :: Symbol) (contains :: Bool) m l r a | ctor l r -> a where
    gmbsProj  :: CLabel ctor -> MutSumF m (GRef_ m l) (GRef_ m r) x -> m (Maybe (Ref m a))
    gmbsEmbed :: CLabel ctor -> Ref m a -> m (MutSumF m (GRef_ m l) (GRef_ m r) x)

instance
      ( PrimMonad m
      , GMutable m r
      , GMutBranchConstructor ctor m l a
      , GL.GIsList (GRef_ m l) (GRef_ m l) (MapRef m as) (MapRef m as)
      , GL.GIsList l l as as
      , GL.ListTuple a as
      , GL.ListTuple b (MapRef m as)
      , Ref m a ~ b
      )
      => GMutBranchSum ctor 'True m l r a where
    gmbsProj lb (MutSumF r) = readMutVar r >>= \case
      L1 x -> gmbcProj lb x
      R1 _ -> pure Nothing
    gmbsEmbed _ = fmap MutSumF . newMutVar . L1 . GLP.view GL.glistR . GL.tupleToList

instance
      ( PrimMonad m
      , GMutable m l
      , GMutBranchConstructor ctor m r a
      , GL.GIsList (GRef_ m r) (GRef_ m r) (MapRef m as) (MapRef m as)
      , GL.GIsList r r as as
      , GL.ListTuple a as
      , GL.ListTuple b (MapRef m as)
      , Ref m a ~ b
      )
      => GMutBranchSum ctor 'False m l r a where
    gmbsProj lb (MutSumF r) = readMutVar r >>= \case
      L1 _ -> pure Nothing
      R1 x -> gmbcProj lb x
    gmbsEmbed _ = fmap MutSumF . newMutVar . R1 . GLP.view GL.glistR . GL.tupleToList

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
-- consMB :: (PrimMonad m, Mutable m a) => MutBranch m [a] (a, [a])
-- consMB = 'constrMB' ('CLabel' @":")
-- @
constrMB
    :: forall ctor m s a.
     ( Ref m s ~ GRef m s
     , GMutBranchConstructor ctor m (Rep s) a
     )
    => CLabel ctor
    -> MutBranch m s a
constrMB l = MutBranch
    { projectBranch = gmbcProj l . unGRef
    , embedBranch   = fmap GRef . gmbcEmbed l
    }

-- | 'MutBranch' focusing on the nil case of a list
nilMB :: (PrimMonad m, Mutable m a) => MutBranch m [a] ()
nilMB = constrMB (CLabel @"[]")

-- | 'MutBranch' focusing on the cons case of a list
consMB :: (PrimMonad m, Mutable m a) => MutBranch m [a] (a, [a])
consMB = constrMB (CLabel @":")

-- | 'MutBranch' focusing on the 'Nothing' case of a 'Maybe'
nothingMB :: (PrimMonad m, Mutable m a) => MutBranch m (Maybe a) ()
nothingMB = constrMB #_Nothing

-- | 'MutBranch' focusing on the 'Just' case of a 'Maybe'
justMB :: (PrimMonad m, Mutable m a) => MutBranch m (Maybe a) a
justMB = constrMB #_Just

-- | 'MutBranch' focusing on the 'Left' case of an 'Either'
leftMB :: (PrimMonad m, Mutable m a, Mutable m b) => MutBranch m (Either a b) a
leftMB = constrMB #_Left

-- | 'MutBranch' focusing on the 'Right' case of an 'Either'
rightMB :: (PrimMonad m, Mutable m a, Mutable m b) => MutBranch m (Either a b) b
rightMB = constrMB #_Right
