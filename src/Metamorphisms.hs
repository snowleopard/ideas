{-# LANGUAGE DeriveFunctor, LambdaCase #-}
module Metamorphisms where

-- Musing on "Metamorphisms: Streaming Representation-Changers" by Jeremy Gibbons
-- http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/metamorphisms-scp.pdf

---------------------- List-to-list metamorphism via trees ---------------------

-- A shape of binary trees with a-labelled leafs, b-labelled internal nodes, and
-- c-typed subtrees.
import Data.Void
data TreeF a b c = Nil | Tip a | Bin c b c deriving Functor

newtype Fix f = Fix { unfix :: f (Fix f) }

-- A tree keeps the `TreeF` shape on every level
type Tree a b = Fix (TreeF a b)

-- If we know how to fold `TreeF a r` into `r`, we can recursively fold a whole tree
foldt :: (TreeF a b r -> r) -> Tree a b -> r
foldt f t = f (foldt f <$> unfix t)

-- Flip the arrows, and now you can unfold a tree from a seed
unfoldt :: (r -> TreeF a b r) -> r -> Tree a b
unfoldt f r = Fix $ unfoldt f <$> f r

-- We don't nead leafs here, so we label them with `Void`
partition :: Ord a => [a] -> TreeF Void a [a]
partition = \case
    []     -> Nil
    a : as -> Bin (filter (<=a) as) a (filter (>a) as)

join :: TreeF Void a [a] -> [a]
join = \case
    Nil       -> []
    Tip x     -> absurd x
    Bin x a y -> x ++ [a] ++ y

-- This is a metamorphism that goes via an intermediate tree. It can't be made
-- streaming: we can't produce the first element until we see the whole list.
quicksort :: Ord a => [a] -> [a]
quicksort = foldt join . unfoldt partition

-- A type of streaming list-to-list metamorphisms that use an internal state `s`.
-- Note that this type differs from the one in the linked paper.
type StreamLL s i o = (i -> s -> ([o], s)) -> (s -> [o]) -> s -> [i] -> [o]

data Acc a = None | One a | Two a a

-- Turn a list of elements into a list of triples of elements. The last element
-- may be incomplete.
streamTriples :: StreamLL (Acc a) a [a]
streamTriples step flush s = \case
    []     -> flush s
    a : as -> case step a s of (res, s) -> res ++ streamTriples step flush s as

-- This is a streaming function, so `take 5 (triples [1..])` works fine.
triples :: [a] -> [[a]]
triples = streamTriples step flush None
  where
    step a = \case
        None    -> ([], One a)
        One x   -> ([], Two x a)
        Two x y -> ([[x, y, a]], None)
    flush = \case
        None -> []
        One x -> [[x]]
        Two x y -> [[x, y]]

---------------------- Tree-to-tree metamorphism via lists ---------------------

data ListF a b = Empty | Cons a b

type List a = Fix (ListF a)

toList :: Tree a () -> [a]
toList = foldt $ \case Nil -> []
                       Tip a -> [a]
                       Bin x () y -> x ++ y

-- Straightforward and boring, a streaming version would be nice
fromList :: [a] -> Tree a ()
fromList = unfoldt $ \case
    []  -> Nil
    [a] -> Tip a
    as  -> let half = length as in Bin (take half as) () (drop half as)
