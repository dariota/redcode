\begin{code}
module Main where

import Worker
import Core
import Executor
import Parser
import Instruction.Instruction

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import System.Environment

import Data.Maybe (fromJust, isNothing)
\end{code}

We want to invoke the MARS by passing in the core size and a list of filenames containing the programs to position in the core before execution.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    let coreSize = read (head args) :: Int
    progTexts <- sequence $ map readFile (tail args)
    let (executors, nCore) = createCore coreSize $ map readProgram progTexts
    context <- createContext nCore
    let workers = map (forkIO . (workHelper context)) executors
    sequence_ workers
    examineTask context (length workers)
\end{code}

Now we need to write the helper functions used to initialise the MARS. We start with the core creation, which creates a core of the specified size, and positions and labels all the programs in it (using the core's own positioning function, which spaces them evenly).

We don't want to worry about unpacking Maybes in main just to throw an error, so we guarantee something will be returned and give a meaningful error message if we can't create the core.

\begin{code}
createCore :: Int -> [[Instruction]] -> ([(Char, Executor)], Core)
createCore size progs = if isNothing tentativeCore then error "Insufficient space for programs in core" else fullCore
    where tentativeCore = positionPrograms (core size) progs
          (execPositions, nCore) = fromJust tentativeCore
          executors = map executor execPositions
          labels = ['A'..]
          fullCore = (zip labels executors, nCore)
\end{code}

Next, we want to create all the required channels and MVar required for the worker threads at once.

\begin{code}
createContext :: Core -> IO (TChan Bool, MVar Core, TChan (Char, Int))
createContext nCore = do
    stepChan <- atomically $ newTChan
    mCore <- newMVar nCore
    taskChan <- atomically $ newTChan
    return (stepChan, mCore, taskChan)
\end{code}

Now we want to conveniently consume that context and produced a curried function expecting the thread specific label and executor, producing an IO action to be passed to forkIO in order to create the actual worker thread.

Note that we need to clone the stepper channel - this is due to the source channel being a broadcast channel (i.e. write-only) and to ensure each worker has its own read pointer within the channel.

\begin{code}
workHelper :: (TChan Bool, MVar Core, TChan (Char, Int)) -> (Char, Executor) -> IO ()
workHelper (step, mCore, taskChan) (label, exec) = do
    myStep <- atomically $ cloneTChan step
    worker myStep mCore taskChan label exec
\end{code}

Lastly, we need to implement examineTask - this allows the user to control the stepping of the workers, printing out how many tasks each worker has remaining after each step of execution.

\begin{code}
examineTask :: (TChan Bool, MVar Core, TChan (Char, Int)) -> Int -> IO ()
examineTask (sigChan, mCore, taskChan) count = forever $ do
    putStrLn "Press enter to step"
    getChar
    atomically $ writeTChan sigChan True
    taskMap <- fillMap count taskChan Map.empty
    core <- takeMVar mCore
    putStrLn $ show $ core
    putMVar mCore core
    putStrLn $ show $ Map.assocs taskMap

fillMap :: Int -> TChan (Char, Int) -> Map.Map Char Int -> IO (Map.Map Char Int)
fillMap 0 _ m = return m
fillMap x tchan m = do
    (label, tasks) <- atomically $ readTChan tchan
    m' <- fillMap (x - 1) tchan m
    return $ Map.insert label tasks m'
\end{code}
