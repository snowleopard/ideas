{-# LANGUAGE ConstraintKinds, LambdaCase #-}
module SelectiveZero where

import Data.Functor

----------------------------------- Selective ----------------------------------
-- The most basic version of the Selective type class.
-- See: https://github.com/snowleopard/selective

class Applicative f => Selective f where
    select :: f (Either a b) -> f (a -> b) -> f b

selectM :: Monad f => f (Either a b) -> f (a -> b) -> f b
selectM mx mf = do
    x <- mx
    case x of
        Left  a -> fmap ($ a) mf
        Right b -> pure b

instance Selective [] where
    select = selectM

--------------------------------- SelectiveZero --------------------------------
-- Extending the Selective type class with the 'zero' method, similar to 'empty'
-- from Alternative.

-- select zero x = zero
class Selective f => SelectiveZero f where
    zero :: f a

instance SelectiveZero [] where
    zero = []

rights :: SelectiveZero f => f (Either a b) -> f b
rights x = select x zero

filter :: SelectiveZero f => (a -> Bool) -> f a -> f a
filter p x = rights $ x <&> \x -> if p x then Right x else Left ()

catMaybes :: SelectiveZero f => f (Maybe a) -> f a
catMaybes x = rights $ x <&> \case { Just x  -> Right x; Nothing -> Left () }

---------------------------------- Witherable ----------------------------------
-- See https://chrispenner.ca/posts/witherable-optics

type Witherable t = (SelectiveZero t, Traversable t)

wither :: (Applicative f, Witherable t) => (a -> f (Maybe b)) -> t a -> f (t b)
wither f t = catMaybes <$> traverse f t
