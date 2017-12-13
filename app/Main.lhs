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

import Text.Read

import Data.Maybe (fromJust, isNothing)
\end{code}

We want to invoke the MARS by passing in the core size, number of steps to take, delay between steps (delay in ms, or manual to manually step the simulation), and a list of filenames containing the programs to position in the core before execution.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    let coreSize = validateCoreSize $ head args
    let steps = validateSteps $ args !! 1
    let delayFunc = getStepFunc $ args !! 2
    progTexts <- sequence $ map readFile (drop 3 args)
    let (executors, nCore) = createCore coreSize $ map readProgram progTexts
    context <- createContext nCore
    let workers = map (forkIO . (workHelper context)) executors
    sequence_ workers
    examineTask context (length workers) steps delayFunc

validateCoreSize :: String -> Int
validateCoreSize = validateInt "Core size"

validateSteps :: String -> Int
validateSteps = validateInt "Step count"

getStepFunc :: String -> IO ()
getStepFunc s = delayFunc
    where delay'' = readMaybe s :: Maybe Int
          delay' = if isNothing delay'' then error "Step function must be a number" else fromJust delay''
          delay = if delay' >= 0 then delay' else error "Step delay must be non-negative"
          manualStep = if s == "manual" then True else False
          delayFunc = if manualStep then manualStepper else threadDelay $ delay * 10 ^ 3

manualStepper :: IO ()
manualStepper = do
    putStrLn "Press enter to step"
    getChar
    return ()

validateInt :: String -> String -> Int
validateInt item s = res
    where res'' = readMaybe s :: Maybe Int
          res' = if isNothing res'' then error $ item ++ " must be a number" else fromJust res''
          res = if res' > 0 then res' else error $ item ++ " must be positive"
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
examineTask :: (TChan Bool, MVar Core, TChan (Char, Int)) -> Int -> Int -> IO () -> IO ()
examineTask _ _ 0 _ = return ()
examineTask ctxt@(sigChan, mCore, taskChan) count steps delayFunc = do
    delayFunc
    atomically $ writeTChan sigChan True
    taskMap <- fillMap count taskChan Map.empty
    core <- takeMVar mCore
    putStrLn $ display core
    putMVar mCore core
    putStrLn $ show $ Map.assocs taskMap
    examineTask ctxt count (steps - 1) delayFunc

fillMap :: Int -> TChan (Char, Int) -> Map.Map Char Int -> IO (Map.Map Char Int)
fillMap 0 _ m = return m
fillMap x tchan m = do
    (label, tasks) <- atomically $ readTChan tchan
    m' <- fillMap (x - 1) tchan m
    return $ Map.insert label tasks m'
\end{code}
