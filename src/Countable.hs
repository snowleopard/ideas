{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables #-}
module Countable where

import Data.Void
import Data.Semigroup (Sum(..))
import Data.Monoid (Any(..), First(..))
import Data.Maybe

-- Countable types whose values can be aggregated in a fixed sequence
class Countable a where
    count :: Monoid m => (a -> m) -> m

instance Countable () where
    count f = f ()

instance Countable Void where
    count _ = mempty

instance Countable Bool where
    count f = f False <> f True

instance (Countable a, Countable b) => Countable (a, b) where
    count f = count $ \a -> count $ \b -> f (a, b)

instance Countable a => Countable (Maybe a) where
    count f = f Nothing <> count (f . Just)

instance (Countable a, Countable b) => Countable (Either a b) where
    count f = count (f . Left) <> count (f . Right)

enumerate :: Countable a => [a]
enumerate = count pure

cardinality :: forall a. Countable a => Int
cardinality = getSum $ count (\(_ :: a) -> Sum 1)

inhabited :: forall a. Countable a => Bool
inhabited = getAny $ count (\(_ :: a) -> Any True)

sat :: Countable a => (a -> Bool) -> Maybe a
sat f = getFirst $ count (\x -> First (if f x then Just x else Nothing))

allSat :: Countable a => (a -> Bool) -> [a]
allSat f = count (\x -> [ x | f x ])

exists :: Countable a => (a -> Bool) -> Bool
exists = isJust . sat

forAll :: Countable a => (a -> Bool) -> Bool
forAll f = not $ exists $ not . f
