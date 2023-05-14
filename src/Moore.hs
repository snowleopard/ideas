module Moore where

import Control.Selective

-- An encoding of Moore machines, showcasing the difference between Selective
-- and Monad instances.
-- Inspired by this tweet: https://twitter.com/fumieval/status/1651776228481961987

-- An internal state and a function to yield an output and move to the next state
data Moore a = forall s. Moore s (s -> (s, a))

instance Functor Moore where
    fmap f (Moore s g) =        -- transform a machine's output
      Moore
        s                       -- no need to change the type of the state
        (\s -> case g s of      -- run the machine
            (s, a) -> (s, f a)) -- return the new state and a transformed output

instance Applicative Moore where
    pure a =                           -- a trivial machine
      Moore
        ()                             -- trivial internal state
        (\() -> ((), a))               -- keep returning the same value

    Moore s x <*> Moore t y =          -- strongly couple two machines
      Moore
        (s, t)                         -- each sub-machine has its own state
        (\(s, t) -> case x s of        -- run 1st machine
            (s, x) -> case y t of      -- run 2nd machine
              (t, y) -> ((s, t), x y)) -- return the new state and the result


instance Selective Moore where
    select (Moore s x) (Moore t y) =     -- weakly couple two machines
      Moore
        (s, t)                           -- each sub-machine has its own state
        (\(s, t) -> case x s of          -- run 1st machine
            (s, Right x) -> ((s, t), x)  -- if the result is right, we're done
            (s, Left  x) -> case y t of  -- otherwise, run 2nd machine
                (t, y) -> ((s, t), y x)) -- return the new state and the result

-- TODO: Implement join and Monad instance.
-- join :: Moore (Moore a) -> Moore a
-- join (Moore s m) = Moore _ _
