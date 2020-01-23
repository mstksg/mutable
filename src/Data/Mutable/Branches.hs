{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}





module Data.Mutable.Branches (
    MutBranch(..)
  , constrMutBranch, CLabel(..)
  , nilMB
  , consMB
  ) where

import           Control.Monad.Primitive
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Primitive.MutVar
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits
import qualified Data.GenericLens.Internal                  as GL
import qualified Data.Generics.Internal.Profunctor.Lens     as GLP



-- | A @'MutBranch' m s a@ represents the information that @s@ could
-- potentially be an @a@.  Similar in spirit to a @Prism' s a@.
--
-- Information to clone out a potential option/branch @a@ of an @s@, and
-- also to copy in a version of @a@ into @s@.
data MutBranch m s a = MutBranch
    { cloneMutBranch :: Ref m s -> m (Maybe (Ref m a))
    , moveMutBranch  :: Ref m s -> Ref m a -> m ()
    }

-- | A version of 'Label' that removes an underscore at the beginning when
-- used with -XOverloadedLabels.  Used to specify constructors, since
-- labels are currently not able to start with capital letters.
data CLabel (ctor :: Symbol) = CLabel

instance (ctor_ ~ AppendSymbol "_" ctor) => IsLabel ctor_ (CLabel ctor) where
    fromLabel = CLabel



class (GMutable m f, Mutable m a) => GMutBranchConstructor (ctor :: Symbol) m f a | ctor f -> a where
    gmbcClone :: CLabel ctor -> GRef_ m f x -> m (Maybe (Ref m a))
    gmbcMove  :: CLabel ctor -> GRef_ m f x -> Ref m a -> m ()

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
    gmbcClone _ = pure
                . Just
                . GL.listToTuple
                . GLP.view GL.glist
                . unM1
    gmbcMove _ = moveRef . GL.listToTuple . GLP.view GL.glist . unM1

instance GMutBranchConstructor ctor m f a => GMutBranchConstructor ctor m (M1 D meta f) a where
    gmbcClone lb = gmbcClone lb . unM1
    gmbcMove  lb = gmbcMove  lb . unM1

instance
      ( PrimMonad m
      , Mutable m a
      , GMutBranchSum ctor (GL.HasCtorP ctor l) m l r a
      )
      => GMutBranchConstructor ctor m (l :+: r) a where
    gmbcClone = gmbsClone @ctor @(GL.HasCtorP ctor l)
    gmbcMove  = gmbsMove @ctor @(GL.HasCtorP ctor l)

class (GMutable m l, GMutable m r, Mutable m a) => GMutBranchSum (ctor :: Symbol) (contains :: Bool) m l r a | ctor l r -> a where
    gmbsClone :: CLabel ctor -> MutSumF m (GRef_ m l) (GRef_ m r) x -> m (Maybe (Ref m a))
    gmbsMove  :: CLabel ctor -> MutSumF m (GRef_ m l) (GRef_ m r) x -> Ref m a -> m ()

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
    gmbsClone lb (MutSumF r) = readMutVar r >>= \case
      L1 x -> gmbcClone lb x
      R1 _ -> pure Nothing
    gmbsMove lb (MutSumF r) a = readMutVar r >>= \case
      L1 x  -> gmbcMove lb x a
      R1 _  -> writeMutVar r . L1 . GLP.view GL.glistR . GL.tupleToList $ a

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
    gmbsClone lb (MutSumF r) = readMutVar r >>= \case
      L1 _ -> pure Nothing
      R1 x -> gmbcClone lb x
    gmbsMove lb (MutSumF r) a = readMutVar r >>= \case
      L1 _  -> writeMutVar r . R1 . GLP.view GL.glistR . GL.tupleToList $ a
      R1 x  -> gmbcMove lb x a

constrMutBranch
    :: forall ctor m s a.
     ( Ref m s ~ GRef m s
     , GMutBranchConstructor ctor m (Rep s) a
     )
    => CLabel ctor
    -> MutBranch m s a
constrMutBranch l = MutBranch
    { cloneMutBranch = gmbcClone l . unGRef
    , moveMutBranch  = gmbcMove  l . unGRef
    }

nilMB :: (PrimMonad m, Mutable m a) => MutBranch m [a] ()
nilMB = constrMutBranch (CLabel @"[]")

consMB :: (PrimMonad m, Mutable m a) => MutBranch m [a] (a, [a])
consMB = constrMutBranch (CLabel @":")


