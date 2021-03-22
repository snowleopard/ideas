{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators, TupleSections #-}
module Components where

import Algebra.Graph.Undirected
import Prelude hiding ((.), id, repeat, round)
import Control.Category
import Control.Monad.Writer
import Data.List.Extra hiding (repeat)
import Data.Map.Strict (Map)
import Data.Void

import qualified Data.Map.Strict as Map

-- Experimenting with a family of Concurrent Connected Components algorithms.
-- See the blog post https://blogs.ncl.ac.uk/andreymokhov/connected-components
-- and the paper https://arxiv.org/pdf/1812.06177.pdf.

-- Type variables `t` and `s` stand for "thread" and "state", respectively,
-- and `i` and `o` are types of incoming and outgoing messages. Note that we can
-- compose global computations simply by using function composition (.), as
-- well as using combinators from "Control.Category".
type Local  s t i o = s -> t -> [i] -> (s, [(t, o)])
type Global s t i o = (Map t s, [(t, i)]) -> (Map t s, [(t, o)])

-- A local computation that does nothing, i.e. `round skip == id`.
skip :: Local s t i i
skip s t i = (s, map (t,) i)

-- A local computation that filters out messages that do not satisfy the given
-- predicate.
filterMessages :: (s -> t -> i -> Bool) -> Local s t i i
filterMessages p s t i = (s, [ (t, m) | m <- i, p s t m ] )

-- Give global-view semantics to a local-view computation round.
round :: Ord t => Local s t i o -> Global s t i o
round local (states, messages) = collect (Map.mapWithKey update states)
  where
    deliveries = Map.fromAscList (groupSort messages)
    update t s = local s t (Map.findWithDefault [] t deliveries)
    collect    = runWriter . traverse writer

-- Repeat a computation until thread states no longer change.
repeat :: (Eq s, Eq t) => Global s t Void Void -> Global s t Void Void
repeat g (states, messages)
    | states == newStates = (states, messages)
    | otherwise           = repeat g (newStates, newMessages)
  where
    (newStates, newMessages) = g (states, messages)

-- Run a global computation extracting the final thread state.
run :: Global s t Void Void -> Map t s -> Map t s
run g m = fst $ g (m, [])

-- Run a global computation extracting the resulting network traffic.
messages :: Global s t Void o -> Map t s -> [(t, o)]
messages g m = snd $ g (m, [])

------------------------ Concurrent Connected Components -----------------------
type Vertex = Int

data Thread = VertexThread Vertex | EdgeThread Vertex Vertex
    deriving (Eq, Ord, Show)

data State = VertexState Vertex | EdgeState
    deriving (Eq, Ord, Show)

type (~>) i o = Global State Thread i o

connect :: Void ~> Vertex
connect = round $ \s t _ -> case t of
    VertexThread _ -> (s, [])
    EdgeThread x y -> (s, [(VertexThread (max x y), min x y)])

update :: Vertex ~> Void
update = round $ \s _ i -> case s of
    VertexState p -> (VertexState $ minimum (p : i), [])
    EdgeState     -> (s, [])

shortcut :: Void ~> Void
shortcut = request >>> respondParent >>> update
  where
    request :: Void ~> Thread
    request = round $ \s t _ -> case s of
        VertexState p -> (s, [(VertexThread p, t)])
        EdgeState     -> (s, [])

respondParent :: Thread ~> Vertex
respondParent = round $ \s _ i -> case s of
    VertexState p -> (s, map (,p) i)
    EdgeState     -> (s, [])

parentConnect :: Void ~> Vertex
parentConnect = request >>> respondParent >>> receive
  where
    request :: Void ~> Thread
    request = round $ \s t _ -> case t of
        VertexThread _ -> (s, [])
        EdgeThread x y -> (s, [(VertexThread x, t), (VertexThread y, t)])

    receive :: Vertex ~> Vertex
    receive = round $ \s _ i -> case s of
        VertexState _ -> (s, [])
        EdgeState     -> case i of
            [x, y] -> (s, [(VertexThread (max x y), min x y)])
            _      -> error "Unexpected number of responses"

rootUpdate :: Vertex ~> Void
rootUpdate = round (filterMessages toRoot) >>> update
  where
    toRoot (VertexState p) (VertexThread v) _ = p == v
    toRoot _ _ _ = False

algorithmP :: Void ~> Void
algorithmP = repeat (parentConnect >>> update >>> shortcut)

algorithmR :: Void ~> Void
algorithmR = repeat (parentConnect >>> rootUpdate >>> shortcut)

initialise :: Graph Int -> Map Thread State
initialise g = Map.fromList (vs ++ es)
  where
    vs = [ (VertexThread x, VertexState x) | x      <- vertexList g ]
    es = [ (EdgeThread x y, EdgeState    ) | (x, y) <- edgeList   g ]

components :: Map Thread State -> [(Int, [Int])]
components m = groupSort
    [ (p, x) | (VertexThread x, VertexState p) <- Map.toList m ]

example :: Graph Int
example = edges
    [ (1, 6)
    , (2, 6)
    , (3, 7)
    , (4, 9)
    , (5, 9)
    , (6, 9)
    , (6, 10)
    , (7, 8)
    , (7, 9)
    , (9, 10)
    , (10, 25)
    , (10, 24)
    , (10, 11)
    , (11, 13)
    , (11, 18)
    , (11, 35)
    , (12, 14)
    , (13, 15)
    , (14, 16)
    , (14, 17)
    , (18, 23)
    , (19, 20)
    , (19, 21)
    , (19, 37)
    , (20, 21)
    , (21, 26)
    , (21, 30)
    , (21, 33)
    , (21, 37)
    , (22, 35)
    , (22, 27)
    , (22, 31)
    , (22, 38)
    , (23, 31)
    , (23, 29)
    , (23, 35)
    , (24, 32)
    , (24, 35)
    , (28, 33)
    , (33, 36)
    , (33, 37)
    , (34, 37)
    , (36, 37) ]
