{-# LANGUAGE TemplateHaskell #-}

module Data.Mutable.Internal.TH (
    mutableTuples
  ) where

import           Control.Monad
import           Data.List
import           Data.Mutable.Internal
import           Language.Haskell.TH

tyVarNames :: [Name]
tyVarNames = mkName . (:[]) <$> filter (/= 's') ['a' .. 'z']

mutableTuples :: [Int] -> Q [Dec]
mutableTuples = traverse mutableTuple

mutableTuple
    :: Int
    -> Q Dec
mutableTuple n = do
    valVars <- replicateM n (newName "x")
    refVars <- replicateM n (newName "r")
    -- instance (Mutable s a, Mutable s b, Mutable s c) => Mutable s (a, b, c) where
    pure $ InstanceD
      Nothing
      (mutableS . VarT <$> tyVars)
      (mutableS instHead)
      [ refImpl
      , thawImpl valVars
      , freezeImpl refVars
      , copyImpl refVars valVars
      , moveImpl refVars valVars
      , cloneImpl refVars
      , unsafeThawImpl valVars
      , unsafeFreezeImpl refVars
      ]
  where
    tuplerT :: [Type] -> Type
    tuplerT = applyAllT (TupleT n)
    tupConE = ConE (tupleDataName n)

    mutableS :: Type -> Type
    mutableS = ((ConT ''Mutable `AppT` VarT (mkName "s")) `AppT`)
    refS :: Type -> Type
    refS = ((ConT ''Ref `AppT` VarT (mkName "s")) `AppT`)
    tyVars :: [Name]
    tyVars = take n tyVarNames
    instHead :: Type
    instHead = tuplerT $ VarT <$> tyVars
    -- type Ref s (a, b, c) = (Ref s a, Ref s b, Ref s c)
    refImpl :: Dec
    refImpl = TySynInstD
            . TySynEqn Nothing (refS instHead)
            $ tuplerT (refS . VarT <$> tyVars)
    -- thawRef   (!x, !y, !z) = (,,) <$> thawRef x   <*> thawRef y   <*> thawRef z
    thawImpl :: [Name] -> Dec
    thawImpl valVars = FunD 'thawRef [
        Clause [TupP (BangP . VarP <$> valVars)]
               (NormalB . liftApplyAllE tupConE $
                  (VarE 'thawRef `AppE`) . VarE <$> valVars
               )
               []
      ]
    -- freezeRef (u , v , w ) = (,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w
    freezeImpl :: [Name] -> Dec
    freezeImpl refVars = FunD 'freezeRef [
        Clause [TupP (VarP <$> refVars)]
               (NormalB . liftApplyAllE tupConE $
                  (VarE 'freezeRef `AppE`) . VarE <$> refVars
               )
               []
      ]
    -- copyRef   (u , v , w ) (!x, !y, !z) = copyRef u x *> copyRef v y *> copyRef w z
    copyImpl :: [Name] -> [Name] -> Dec
    copyImpl refVars valVars = FunD 'copyRef [
        Clause [ TupP (BangP . VarP <$> refVars)
               , TupP (BangP . VarP <$> valVars)
               ]
               (NormalB . sequenceAllE $
                  zipWith (\r v -> (VarE 'copyRef `AppE` VarE r) `AppE` VarE v) refVars valVars
               )
               []
      ]
    -- moveRef   (u , v , w ) ( x,  y,  z) = moveRef u x *> moveRef v y *> moveRef w z
    moveImpl :: [Name] -> [Name] -> Dec
    moveImpl refVars valVars = FunD 'moveRef [
        Clause [ TupP (BangP . VarP <$> refVars)
               , TupP (BangP . VarP <$> valVars)
               ]
               (NormalB . sequenceAllE $
                  zipWith (\r v -> (VarE 'moveRef `AppE` VarE r) `AppE` VarE v) refVars valVars
               )
               []
      ]
    -- cloneRef  (u , v , w ) = (,,) <$> cloneRef u   <*> cloneRef v   <*> cloneRef w
    cloneImpl :: [Name] -> Dec
    cloneImpl refVars = FunD 'cloneRef [
        Clause [TupP (VarP <$> refVars)]
               (NormalB . liftApplyAllE tupConE $
                  (VarE 'cloneRef `AppE`) . VarE <$> refVars
               )
               []
      ]
    -- unsafeThawRef   (!x, !y, !z) = (,,) <$> unsafeThawRef x   <*> unsafeThawRef y   <*> unsafeThawRef z
    unsafeThawImpl :: [Name] -> Dec
    unsafeThawImpl valVars = FunD 'unsafeThawRef [
        Clause [TupP (BangP . VarP <$> valVars)]
               (NormalB . liftApplyAllE tupConE $
                  (VarE 'unsafeThawRef `AppE`) . VarE <$> valVars
               )
               []
      ]
    -- unsafeFreezeRef (u , v , w ) = (,,) <$> unsafeFreezeRef u <*> unsafeFreezeRef v <*> unsafeFreezeRef w
    unsafeFreezeImpl :: [Name] -> Dec
    unsafeFreezeImpl refVars = FunD 'unsafeFreezeRef [
        Clause [TupP (VarP <$> refVars)]
               (NormalB . liftApplyAllE tupConE $
                  (VarE 'unsafeFreezeRef `AppE`) . VarE <$> refVars
               )
               []
      ]

applyAllT
    :: Type
    -> [Type]
    -> Type
applyAllT = foldl' (\t m -> t `AppT` m)

-- | liftApplyAllE f [x,y,z] = f <$> x <*> y <*> z
liftApplyAllE
    :: Exp
    -> [Exp]
    -> Exp
liftApplyAllE = foldl' (\t m -> (VarE '(<*>) `AppE` t) `AppE` m)
              . (VarE 'pure `AppE`)

-- | liftApplyAllE f [x,y,z] = f <$> x <*> y <*> z
sequenceAllE
    :: [Exp]
    -> Exp
sequenceAllE = foldr1 (\x y -> (VarE '(*>) `AppE` x) `AppE` y)


-- applyAllE
--     :: Exp
--     -> [Exp]
--     -> Exp
-- applyAllE = foldl' (\t m -> t `AppE` m)






