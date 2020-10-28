import Control.Applicative
import Data.Void
import Data.Semigroup
import Data.Maybe

-- A CPS monad inspired generalisation of existential and universal quantifiers
class Aggregate a where
    aggregate :: Alternative f => (a -> f r) -> f r

instance Aggregate () where
    aggregate f = f ()

instance Aggregate Void where
    aggregate _ = empty

instance Aggregate Bool where
    aggregate f = f False <|> f True

instance (Aggregate a, Aggregate b) => Aggregate (a, b) where
    aggregate f = aggregate $ \a -> aggregate $ \b -> f (a, b)

instance (Aggregate a, Aggregate b) => Aggregate (Either a b) where
    aggregate f = aggregate (f . Left) <|> aggregate (f . Right)

sat :: Aggregate a => (a -> Bool) -> Maybe a
sat f = getOption $ aggregate (\x -> Option (if f x then Just x else Nothing))

allSat :: Aggregate a => (a -> Bool) -> [a]
allSat f = aggregate (\x -> [ x | f x ])

exists :: Aggregate a => (a -> Bool) -> Bool
exists = isJust . sat

forall :: Aggregate a => (a -> Bool) -> Bool
forall f = not $ exists $ not . f
