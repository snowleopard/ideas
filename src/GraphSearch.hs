{-# LANGUAGE TupleSections, FlexibleContexts  #-}
module GraphSearch where

import qualified Data.Set as Set

import Control.Monad.State
import Data.List (foldl')
import Data.Tree
import Control.Monad (filterM, when)

-- Experimenting with general and reusable interfaces for DFS and BFS graph
-- search algorithms. Inspired by conversations with Brent Yorgey.

dfs :: Ord a => (a -> [r] -> r) -> (a -> [a]) -> a -> r
dfs f g a = snd (visit Set.empty a)
  where
    visit visited a = (newVisited, f a rs)
      where
        (newVisited, rs) = foldl' go (Set.insert a visited, []) (g a)
        go (visited, rs) a
            | Set.member a visited = (visited, rs)
            | otherwise = (newVisited, r : rs)
                            where
                              (newVisited, r) = visit visited a

dfsTree :: Ord a => (a -> [a]) -> a -> Tree a
dfsTree = dfs Node

bfs :: Ord a => (a -> [r] -> r) -> (a -> [a]) -> a -> r
bfs f g a = fromTree $ evalState (explore a) (Set.singleton a)
  where
    fromTree (Node a forest) = f a (map fromTree forest)
    explore = unfoldTreeM_BF walk
    walk a = (a,) <$> neighbours a
    neighbours a = filterM discovered (g a)
    discovered a = do new <- gets (not . Set.member a)
                      when new $ modify (Set.insert a)
                      return new

bfsTree :: Ord a => (a -> [a]) -> a -> Tree a
bfsTree = bfs Node

{- Example:

   λ> let graph v = case v of { 1 -> [2, 3]; 2 -> [3, 4]; 3 -> [4, 5, 6, 2]; _ -> [] }
   λ> putStrLn $ drawTree (show <$> bfsTree graph 1)
   1
   |
   +- 2
   |  |
   |  `- 4
   |
   `- 3
     |
     +- 5
     |
     `- 6

   λ> putStrLn $ drawTree (show <$> dfsTree graph 1)
   1
   |
   `- 2
     |
     `- 3
         |
         +- 6
         |
         +- 5
         |
         `- 4
-}
