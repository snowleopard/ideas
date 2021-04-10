{-# LANGUAGE RankNTypes, LambdaCase, GADTs, ScopedTypeVariables #-}
module Async where

-- A basic concurrency monad, following the classic ideas from:
-- "A Poor Man's Concurrency Monad" by Koen Claessen (1999)
-- "A Monad for Deterministic Parallelism" by Simon Marlow et al (2011)

import Data.IORef
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Control.Monad.Reader

------------------------------ Jobs and job queue ------------------------------
-- A "job" is an IO computation that has access to a job queue, so it can spawn
-- new jobs or suspend itself.
type Job = ReaderT JobQueue IO ()

-- A job queue is a mutable reference to a sequence of jobs. We will be using a
-- simple round-robin scheduling, so jobs will be added at the end and removed
-- from the front of the sequence.
newtype JobQueue = JobQueue (IORef (Seq Job))

enqueue :: Job -> JobQueue -> IO ()
enqueue job (JobQueue ref) = modifyIORef ref (|> job)

dequeue :: JobQueue -> IO (Maybe Job)
dequeue (JobQueue ref) = atomicModifyIORef ref $ \case
    Empty -> (Empty, Nothing)
    job :<| jobs -> (jobs, Just job)

-- A friendly-named alias for running a queue accessing IO computation as a job.
withJobQueue :: (JobQueue -> IO a) -> ReaderT JobQueue IO a
withJobQueue = ReaderT

---------------------------------- Async monad ---------------------------------
-- We could have equivalently defined 'Async' using the standard continuation
-- monad transformer 'ContT', to avoid writing the instances manually:
--
--   type Async a = ContT () (ReaderT JobQueue IO) a
--
-- However, working things out explicitly seems more fun.
newtype Async a = Async ((a -> Job) -> Job)

instance Functor Async where
    fmap f (Async x) = Async $ \k -> x (k . f)

instance Applicative Async where
    pure a = Async $ \k -> k a
    Async f <*> Async x = Async $ \k -> f (\g -> x (k . g))

instance Monad Async where
    return = pure
    Async x >>= f = Async $ \k -> x (\a -> let Async y = f a in y k)

-- Lift an IO computation into the Async monad
atom :: IO a -> Async a
atom io = Async $ \k -> lift io >>= k

asyncJob :: Async () -> Job
asyncJob (Async f) = f return

suspend :: Async () -> Async ()
suspend async = Async $ \_ -> withJobQueue (enqueue (asyncJob async))

fork :: Async () -> Async ()
fork async = Async $ \k -> withJobQueue (enqueue (asyncJob async)) >>= k

spawn :: Async a -> Async (IVar a)
spawn x = do
    res <- newIVar
    fork $ x >>= writeIVar res
    return res

------------------------------ Immutable variables -----------------------------
-- An "immutable variable" that can be written to once and read multiple times.
-- It starts its life with the empty waiting list of callbacks '[a -> Job ()]'
-- and accumulates them as new reads occur. After a write, all the callbacks are
-- executed and the state switches to 'Value a'.
newtype IVar a = IVar (IORef (IVarState a))

data IVarState a = WaitingList [a -> Job] | Value a

newIVar :: Async (IVar a)
newIVar = Async $ \k -> do
    ref <- lift $ newIORef (WaitingList [])
    k (IVar ref)

readIVar :: IVar a -> Async a
readIVar (IVar ref) = Async $ \k -> join $ lift $ atomicModifyIORef ref $ \case
    WaitingList ks -> (WaitingList (k : ks), return ())
    value@(Value a) -> (value, k a)

writeIVar :: IVar a -> a -> Async ()
writeIVar (IVar ref) a = Async $ \k -> join $ lift $ atomicModifyIORef ref $ \case
    WaitingList ks -> (Value a, sequence_ [ k a | k <- reverse ks ] >> k ())
    Value _ -> error "IVar written more than once"

----------------------------------- Execution ----------------------------------
runJobs :: Job
runJobs = withJobQueue dequeue >>= \case
    Nothing -> return ()
    Just job -> job >> runJobs

-- Run an asynchronous computation and return 'Nothing' if it deadlocks.
run :: Async a -> IO (Maybe a)
run async = do
    resultVar <- newIORef (WaitingList [])
    let job = asyncJob $ async >>= writeIVar (IVar resultVar)
    jobQueue <- JobQueue <$> newIORef (Seq.singleton job)
    runReaderT runJobs jobQueue
    result <- readIORef resultVar
    return $ case result of
        WaitingList _ -> Nothing
        Value a -> Just a

runAndShowResult :: Show a => Async a -> IO ()
runAndShowResult async = do
    res <- run async
    case res of
        Nothing -> putStrLn "Deadlock"
        Just res -> putStrLn $ "Result = " ++ show res

-------------------------------- Synchronisation -------------------------------
-- Execute two async computations in sequence and collect their results.
sequentially :: Async a -> Async b -> Async (a, b)
sequentially x y = do
    a <- x
    b <- y
    return (a, b)

-- Execute the async computations concurrently and collect their results.
concurrently :: Async a -> Async b -> Async (a, b)
concurrently x y = do
    a <- spawn x
    b <- spawn y
    sequentially (readIVar a) (readIVar b)

-- A program showing the difference between sequential and concurrent execution.
-- If you replace 'concurrently' with 'sequentially', the program will deadlock.
greeting :: IO ()
greeting = runAndShowResult $ do
    var <- newIVar
    let greet = do
            name <- readIVar var
            atom $ putStrLn $ "Hello, " ++ name ++ "!"
        input = do
            name <- atom getLine
            writeIVar var name
            return name
    concurrently greet input

------------------------------- Non DI primitives ------------------------------
-- 'Async' programs that use the above primitives are reminiscent of so-called
-- "delay insensitive" (DI) asynchronous circuits. Here we introduce 'peekIVar',
-- which provides a non-blocking access to an 'IVar'. Programs that use it would
-- belong to the "speed independent" class of asynchronous circuits.
peekIVar :: IVar a -> Async (Maybe a)
peekIVar (IVar ref) = Async $ \k -> lift (readIORef ref) >>= \case
    WaitingList _ -> k Nothing
    Value a -> k (Just a)

-- This example demonstrates that the behaviour of programs that use 'peekIVar'
-- can depend on job scheduling. If you change 'concurrently input check' to
-- 'concurrently check input', the result of the check will become 'Nothing'.
greetingWithPeek :: IO ()
greetingWithPeek = runAndShowResult $ do
    var <- newIVar
    let greet = do
            name <- readIVar var
            atom $ putStrLn $ "Hello, " ++ name ++ "!"
        input = do
            name <- atom getLine
            writeIVar var name
            return name
        check = do
           name <- peekIVar var
           atom $ putStrLn $ "Checking the current state: " ++ show name
           return name
    concurrently greet (concurrently input check)
