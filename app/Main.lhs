\begin{code}
module Main where

import Worker
import Core
import Executor
import Parser
import Instruction.Instruction

import Control.Monad (join)
import Control.Monad.Trans.State (runState)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import qualified Data.Map.Strict as Map (Map, assocs, empty, insert, insertWith, fromList)
import System.Environment (getArgs)

import Text.Read (readMaybe)

import Data.Maybe (fromJust, isNothing)
\end{code}

We want to invoke the MARS by passing in the core size, number of steps to take, delay between steps (delay in ms, or manual to manually step the simulation), and a list of filenames containing the programs to position in the core before execution.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    let (cSize, steps, delayFunc, progFiles) = readArgs args
    progTexts <- sequence $ map readFile progFiles
    (executors, nCore) <- createCore cSize $ map readProgram progTexts
    context <- createContext nCore
    let workers = map (forkIO . (workHelper context)) executors
    sequence_ workers
    examineTask context (length workers) steps delayFunc

-- helper functions to read inputs from CLI args
readArgs :: [String] -> (Int, Int, IO (), [FilePath])
readArgs args = (coreSize, paths)
    where coreSize = validateCoreSize $ head args
          paths = tail args

validateCoreSize :: String -> Int
validateCoreSize = validateInt "Core size"

validateInt :: String -> String -> Int
validateInt item s = if res > 0 then res else error $ item ++ " must be positive"
    where maybeSize = readMaybe s :: Maybe Int
          res = if isNothing maybeSize then error $ item ++ " must be a number" else fromJust maybeSize
\end{code}

Now we need to write the helper functions used to initialise the MARS. We start with the core creation, which creates a core of the specified size, and positions and labels all the programs in it (using the core's own positioning function, which spaces them evenly).

\begin{code}
createCore :: Monad m => Int -> [[Instruction]] -> m ([(Char, Executor)], Mars)
createCore size progs = return $ runState (do
    positions <- positionPrograms progs
    let executors = map executor (join positions)
    pure $ zip ['A'..] executors) (core size)
\end{code}

Next, we want to create all the required channels and MVar required for the worker threads at once.

\begin{code}
createContext :: Mars -> IO (TChan Bool, MVar Mars, TChan (Char, [Int]))
createContext nCore = do
    stepChan <- atomically $ newTChan
    mCore <- newMVar nCore
    taskChan <- atomically $ newTChan
    return (stepChan, mCore, taskChan)
\end{code}

Now we want to conveniently consume that context and produced a curried function expecting the thread specific label and executor, producing an IO action to be passed to forkIO in order to create the actual worker thread.

Note that we need to clone the stepper channel - this is due to the source channel being a broadcast channel (i.e. write-only) and to ensure each worker has its own read pointer within the channel.

\begin{code}
workHelper :: (TChan Bool, MVar Mars, TChan (Char, [Int])) -> (Char, Executor) -> IO ()
workHelper (step, mCore, taskChan) (label, exec) = do
    myStep <- atomically $ cloneTChan step
    worker myStep mCore taskChan label exec
\end{code}

Lastly, we need to implement examineTask - this allows the user to control the stepping of the workers, printing out how many tasks each worker has remaining after each step of execution.

\begin{code}
examineTask :: (TChan Bool, MVar Mars, TChan (Char, Int)) -> Int -> Int -> IO () -> IO ()
examineTask _ _ 0 _ = putStrLn "Time's up, no winner!"
examineTask ctxt@(sigChan, mCore, taskChan) count steps delayFunc = do
    delayFunc
    atomically $ writeTChan sigChan True
    taskMap <- fillMap count taskChan Map.empty
    core <- takeMVar mCore
    putStrLn $ display core
    putMVar mCore core
    let mapEntries = Map.assocs taskMap
    putStrLn $ show mapEntries
    putStrLn $ "\n" ++ (show (steps - 1)) ++ " steps remaining\n"
    let remainingTasks = filter (\(_, y) -> y /= 0) mapEntries
    if length remainingTasks == 1 then
        putStrLn $ [fst $ head remainingTasks] ++ " wins!"
    else
        examineTask ctxt count (steps - 1) delayFunc

getChanMap :: Int -> TChan (Char, [Int]) -> IO (Map.Map Char [Int])
getChanMap = fillMap Map.empty

fillMap :: Map.Map Char [Int] -> Int -> TChan (Char, [Int]) -> IO (Map.Map Char [Int])
fillMap m 0 _ = return m
fillMap m x tchan = do
    (label, tasks) <- atomically $ readTChan tchan
    m' <- fillMap m (x - 1) tchan
    return $ Map.insert label tasks m'
\end{code}
