{-# LANGUAGE LambdaCase #-}
module Lem where

import Data.Void

type Not a = a -> Void

-- An encoding of the law of excluded middle (¬A \/ A)
class Lem a where
    lem :: Either (Not a) a

instance Lem () where
    lem = Right ()

instance Lem Void where
    lem = Left absurd

instance Lem Bool where
    lem = Right True

instance (Lem a, Lem b) => Lem (a, b) where
    lem = case (lem, lem) of
        (Left notA, _        ) -> Left (\(a, _) -> notA a)
        (_        , Left notB) -> Left (\(_, b) -> notB b)
        (Right a  , Right b  ) -> Right (a, b)

instance (Lem a, Lem b) => Lem (Either a b) where
    lem = case (lem, lem) of
        (Right a  , _        ) -> Right (Left a)
        (_        , Right b  ) -> Right (Right b)
        (Left notA, Left notB) -> Left $ \case Left  a -> notA a
                                               Right b -> notB b

instance (Lem a, Lem b) => Lem (a -> b) where
    lem = case (lem, lem) of
        (Left notA, _        ) -> Right (absurd . notA)
        (Right _  , Right b  ) -> Right (const b)
        (Right a  , Left notB) -> Left (\f -> notB (f a))

-- Double negation
dne :: Lem a => Not (Not a) -> a
dne notNotA = case lem of
    Left notA -> absurd (notNotA notA)
    Right a   -> a

-- Peirce's law
peirce :: Lem a => ((a -> b) -> a) -> a
peirce f = case lem of
    Left notA -> f (absurd . notA)
    Right a   -> a
