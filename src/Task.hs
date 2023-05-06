{-# LANGUAGE ConstraintKinds, ApplicativeDo, ImpredicativeTypes #-}
module Task where

import Data.Functor
import Control.Selective

-- A variation on an idea for describing tasks; see [1,2] for more details.
-- [1] https://blogs.ncl.ac.uk/andreymokhov/the-task-abstraction/
-- [2] Build Systems à la Carte: https://doi.org/10.1017/S0956796820000088

-- 'Task' types don't require ImpredicativeTypes
type Task c k v a = forall f. c f => (k -> f v) -> f a

type TaskF k v a = forall f. Functor     f => (k -> f v) -> f a
type TaskA k v a = forall f. Applicative f => (k -> f v) -> f a
type TaskS k v a = forall f. Selective   f => (k -> f v) -> f a
type TaskM k v a = forall f. Monad       f => (k -> f v) -> f a

-- 'Tasks' types require ImpredicativeTypes due to the forall in 'Task'
type Tasks c k v a = k -> Maybe (Task c k v a)

type TasksF k v a = k -> Maybe (TaskF k v a)
type TasksA k v a = k -> Maybe (TaskA k v a)
type TasksS k v a = k -> Maybe (TaskS k v a)
type TasksM k v a = k -> Maybe (TaskM k v a)

--------------------------------- Example tasks --------------------------------

taskF :: Task Functor String Integer Bool
taskF fetch = do
    a <- fetch "A"
    return (a < 0)

taskA :: Task Applicative String Integer Bool
taskA fetch = do
    a <- fetch "A"
    b <- fetch "B"
    return (a < b)

taskS :: Task Selective String Integer Bool
taskS fetch = do
    a <- ifS (fetch "A" <&> (> 0)) (fetch "X") (fetch "Y")
    b <- fetch "B"
    return (a < b)

taskM :: Task Monad String Integer Bool
taskM fetch = do
    a <- fetch "A"
    b <- fetch (show a)
    return (b < 0)

------------------------ Mixing different tasks together -----------------------

tasksF :: Tasks Functor String Integer Bool
tasksF "x" = Just taskF
tasksF _   = Nothing

tasksA :: Tasks Applicative String Integer Bool
tasksA "x" = Just taskF
tasksA "y" = Just taskA
tasksA _   = Nothing

tasksS :: Tasks Selective String Integer Bool
tasksS "x" = Just taskF
tasksS "y" = Just taskA
tasksS "z" = Just taskS
tasksS _   = Nothing

-- Can't use 'taskS' here because Selective is not a superclass of Monad
tasksM :: Tasks Monad String Integer Bool
tasksM "x" = Just taskF
tasksM "y" = Just taskA
tasksM "z" = Just taskM
tasksM _   = Nothing
