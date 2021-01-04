{-# LANGUAGE LambdaCase #-}
module Early where

-- Experiments with composable monads following the ideas from "Composing monads"
-- by Mark Jones and Luc Duponcheel [1], and "Early return for do-expressions" by
-- Chris Done [2].
-- [1] http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf.
-- [2] https://github.com/inflex-io/early.

import Control.Monad (join)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Bifunctor

{-# ANN module "HLint: ignore Use =<<" #-}

class Functor f => Pointed f where
    point :: a -> f a

instance (Pointed f, Pointed g) => Pointed (Compose f g) where
    point = Compose . point . point

class Early m where
    early :: Pointed f => m (f (m a)) -> f (m a)

instance Early Identity where
    early = runIdentity

instance Early Maybe where
    early = \case
        Nothing -> point Nothing
        Just x -> x

instance Early (Either e) where
    early = \case
        Left e -> point (Left e)
        Right x -> x

instance Monoid w => Early ((,) w) where
    early (w, f) = first (w <>) <$> f

returnEarly :: (Pointed m, Pointed f) => a -> m (f a)
returnEarly = point . point

fmapEarly :: (Functor m, Functor f) => (a -> b) -> m (f a) -> m (f b)
fmapEarly f = fmap (fmap f)

-- Alas, Pointed is not a superclass of Monad...
joinEarly :: (Pointed m, Monad m, Early f) => m (f (m (f a))) -> m (f a)
joinEarly = join . fmap early

bindEarly :: (Pointed m, Monad m, Early f, Functor f) => m (f a) -> (a -> m (f b)) -> m (f b)
bindEarly x f = joinEarly (fmapEarly f x)

(>>=?) :: (Pointed m, Monad m, Early f, Functor f) => m (f a) -> (a -> m (f b)) -> m (f b)
(>>=?) = bindEarly

(>>?) :: (Pointed m, Monad m, Early f, Functor f) => m (f a) -> m (f b) -> m (f b)
x >>? y = x >>=? const y

-- No idea if this works but seems plausible
instance (Monad f, Pointed f, Pointed g, Early f, Early g) => Early (Compose f g) where
    early = fmap (Compose . joinEarly)
          . early
          . fmap (fmap point . early)
          . getCompose
          . fmap (fmap (point . getCompose))
