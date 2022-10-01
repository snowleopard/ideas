{-# LANGUAGE RankNTypes, DerivingVia, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- See the Show instance

module Nested where

-- I came across nested functors a few times, usually in the context of
-- acquiring/releasing resources. You can find a few nice examples of
-- nested IO computations in this blog post by Gabriella Gonzalez:
--
-- https://www.haskellforall.com/2018/02/the-wizard-monoid.html
--
-- It's unclear if there is anything particularly interesting about nested
-- functors. Perhaps, they are just a special case of 'Compose'. The point
-- of this experiment is to find some interesting instances that are not
-- possible for general @Compose f g@.

import Data.Functor
import Data.Functor.Compose
import Data.Monoid
import Control.Monad

-- | A nested @f@-shaped computation.
--
-- Nested computations can be safely (i.e. without accidentally mixing up the
-- two layers) manipulated via the 'Functor' and 'Applicative' instances.
--
-- We can't have the @Monad f => Monad (Nested f a)@ instance, however, because
-- monads are not composable in general. You can do it for some @f@'s though.
--
-- I think you can't have a 'Selective' instance either, for the same reason,
-- but here I'm less confident. The story of composing selective functors has
-- not been fully explored yet.
newtype Nested f a = Nested { getNested :: f (f a) }
    deriving (Functor, Applicative) via Compose f f
    deriving (Semigroup, Monoid) via Ap (Compose f f) a

-- | Once you're done constructing a nested computation, you can run it by
-- collapsing the two computation layers with the monadic 'join'.
run :: Monad m => Nested m a -> m a
run = join . getNested

------------------------------------ Wizards -----------------------------------

type Wizard a = Nested IO a

-- This suggest that an alternative representation for nested computations:
-- newtype Nested f a = Nested { request :: f x; response :: x -> f b }
wizard :: IO a -> (a -> IO b) -> Wizard b
wizard request response = Nested (request <&> response)

name :: Wizard ()
name = Nested $ do
    putStrLn "What is your name?"
    x <- getLine
    return (putStrLn ("Your name is: " ++ x))

age :: Wizard ()
age = Nested $ do
    putStrLn "What is your age?"
    x <- getLine
    return (putStrLn ("Your age is: " ++ x))


---------------------------- Less interesting stuff ----------------------------

-- Curiously, this requires UndecidableInstances. I suppose the problem is that
-- the instance resolution algorithm might loop, doubling f's over and over.
deriving instance Show (f (f a)) => Show (Nested f a)

-- Violates the Right Identity law:
--
--   m >>= return ≡ m
--
-- Indeed, bind1 (Nested [[1,2]]) = Nested [[1], [2]]
bind1 :: Monad m => Nested m a -> (a -> Nested m b) -> Nested m b
bind1 (Nested x) f = Nested $ join x >>= (getNested . f)

-- Violates the Right Identity law:
--
--   m >>= return ≡ m
--
-- Indeed, bind2 (Nested [[1,2]]) = Nested [[1], [2]]
bind2 :: Monad m => Nested m a -> (a -> Nested m b) -> Nested m b
bind2 x f = Nested $ join $ join $ getNested $ fmap (getNested . f) x
