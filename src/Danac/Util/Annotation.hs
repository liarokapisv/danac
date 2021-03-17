{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Danac.Util.Annotation where

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Show
import Data.Comp.Multi.Term
import Control.Monad (liftM)

data (f :&: a) (g ::  * -> *) e = f g e :&: a
pattern x :&.: y = Term (x :&: y)

getAnn :: (f :&: a) g e -> a
getAnn (_ :&: x) = x

getVal :: (f :&: a) g e -> f g e
getVal (x :&: _) = x

getTAnn :: Term (f :&: a) e -> a
getTAnn = getAnn . unTerm

getTVal :: Term (f :&: a) e -> f (Term (f :&: a)) e
getTVal = getVal . unTerm

instance (HFunctor f) => HFunctor (f :&: a) where
    hfmap f (v :&: c) = hfmap f v :&: c

instance (HFoldable f) => HFoldable (f :&: a) where
    hfold (v :&: _) = hfold v
    hfoldMap f (v :&: _) = hfoldMap f v
    hfoldr f e (v :&: _) = hfoldr f e v
    hfoldl f e (v :&: _) = hfoldl f e v
    hfoldr1 f (v :&: _) = hfoldr1 f v
    hfoldl1 f (v :&: _) = hfoldl1 f v

instance (HTraversable f) => HTraversable (f :&: a) where
    htraverse f (v :&: c) =  (:&: c) <$> (htraverse f v)
    hmapM f (v :&: c) = liftM (:&: c) (hmapM f v)

instance (ShowHF f, Show a) => ShowHF (f :&: a) where
    showHF (v :&: p) =  K $ "(" ++ unK (showHF v) ++ " :&: " ++ show p ++ ")"

data (f :&&: a) (g ::  * -> *) e = f g e :&&: a e
pattern x :&&.: y = Term (x :&&: y)

getAnnI :: (f :&&: a) g e -> a e
getAnnI (_ :&&: x) = x

getValI :: (f :&&: a) g e -> f g e
getValI (x :&&: _) = x

getTAnnI :: Term (f :&&: a) e -> a e
getTAnnI = getAnnI . unTerm

getTValI :: Term (f :&&: a) e -> f (Term (f :&&: a)) e
getTValI = getValI . unTerm

instance (HFunctor f) => HFunctor (f :&&: a) where
    hfmap f (v :&&: c) = hfmap f v :&&: c

instance (HFoldable f) => HFoldable (f :&&: a) where
    hfold (v :&&: _) = hfold v
    hfoldMap f (v :&&: _) = hfoldMap f v
    hfoldr f e (v :&&: _) = hfoldr f e v
    hfoldl f e (v :&&: _) = hfoldl f e v
    hfoldr1 f (v :&&: _) = hfoldr1 f v
    hfoldl1 f (v :&&: _) = hfoldl1 f v

instance (HTraversable f) => HTraversable (f :&&: a) where
    htraverse f (v :&&: c) =  (:&&: c) <$> (htraverse f v)
    hmapM f (v :&&: c) = liftM (:&&: c) (hmapM f v)

instance (ShowHF f, forall e. Show (a e)) => ShowHF (f :&&: a) where
    showHF (v :&&: p) =  K $ "(" ++ unK (showHF v) ++ " :&&: " ++ show p ++ ")"

strip :: HFunctor f => Term (f :&: a) e -> Term f e
strip = cata go
    where go (x :&: _) = Term x

stripI :: HFunctor f => Term (f :&&: a) e -> Term f e
stripI = cata go
    where go (x :&&: _) = Term x

evolve :: HFunctor f => Term (f :&: a) e -> Term (f :&&: K a) e
evolve = cata go
    where go (x :&: y) = (x :&&.: K y)

regress :: HFunctor f => Term (f :&&: K a) e -> Term (f :&: a) e
regress = cata go
    where go (x :&&: K y) = (x :&.: y)
