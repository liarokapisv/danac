{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Danac.Util.Annotation where

import Data.Comp.Multi.Algebra
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.Show
import Data.Comp.Multi.Term
import Control.Monad (liftM)
import Data.Comp.Multi.Ops (fsnd, (:*:)(..))
import Data.Functor

data (f :&: a) (g ::  * -> *) e = !(f g e) :&: !a
pattern x :&.: y = Term (x :&: y)
{-# COMPLETE (:&.:) #-}

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
    showHF (v :&: p) =  case show p of
                                "" -> showHF v
                                x -> K $ "(" ++ unK (showHF v) ++ " :&: " ++ x ++ ")"

data (f :&&: a) (g ::  * -> *) e = !(f g e) :&&: !(a e)
pattern x :&&.: y = Term (x :&&: y)
{-# COMPLETE (:&&.:) #-}

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
    showHF (v :&&: p) = case show p of
                                "" -> showHF v
                                x ->  K $ "(" ++ unK (showHF v) ++ " :&&: " ++ x ++ ")"

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

newtype F f (a :: * -> *) i = F { unF :: f (a i) }

extendAlg :: forall f t a e . (Applicative f, HTraversable t) => (forall k l . f k -> (k -> f l) -> f l) -> Alg (t :&&: a) (F f e) -> Alg (t :&&: a) (F f (Term (t :&&: (a :*: e))))
extendAlg bindF alg (x :&&: ann) = let lhs = htraverse unF x
                                    in F $ bindF lhs $ \lhs' -> getEval ann lhs' <&> \e -> 
                                            lhs' :&&.: (ann :*: e)
    where getEval :: a i -> t (Term (t :&&: (a :*: e))) i -> f (e i)
          getEval a =  unF . alg . (:&&: a) . hfmap (F . (pure @ f) . fsnd . getTAnnI)

extend :: (Applicative f, HTraversable t) => (forall k l. f k -> (k -> f l) -> f l) -> Alg (t :&&: a) (F f e) -> Term (t :&&: a) i -> F f (Term (t :&&: (a :*: e))) i
extend b f = cata (extendAlg b f)

combineAlg :: (forall i . a i -> b i -> c i) -> Alg (t :&&: (a :*: b)) (Term (t :&&: c))
combineAlg f (x :&&: (y :*: z)) = x :&&.: f y z

combine :: HFunctor t => (forall j . a j -> b j -> c j) -> Term (t :&&: (a :*: b)) i -> Term (t :&&: c) i
combine f = cata (combineAlg f)
