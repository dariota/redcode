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
import Data.List (sort)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
\end{code}

We want to invoke the MARS by passing in the core size, and a list of filenames containing the programs to position in the core before execution. These will each run in their own thread and be externally synchronised, while the results of their execution are displayed in the GUI.

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    let (cSize, progFiles) = readArgs args
    progTexts <- sequence $ map readFile progFiles
    (executors, nCore) <- createCore cSize $ map readProgram progTexts
    context <- createContext nCore
    let workers = map (forkIO . (workHelper context)) executors
    sequence_ workers
    runGUI context (length workers)

-- helper functions to read inputs from CLI args
readArgs :: [String] -> (Int, [FilePath])
readArgs args  = (coreSize, paths)
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

We now need to implement runGUI - this presents a threepenny-gui for the user to step through the program, viewing where the tasks currently are and what the instructions in the core are.

The GUI will consist of a single button which takes one step through the simulation, and a table showing all the instructions in the core, along with a list of what instruction tasks in the various programs are currently executing.

\begin{code}
runGUI :: (TChan Bool, MVar Mars, TChan (Char, [Int])) -> Int -> IO ()
runGUI ctxt@(sigChan, mCore, taskChan) count = startGUI defaultConfig $ \w -> do
    return w # set title "MARS"

    initialCore <- liftIO $ readMVar mCore

    step <- UI.button # set text ("Step")
    coreTab <- UI.table #+ (tableHeader : (tableContents initialCore []))

    stepBehaviour <- accumB mCore (const mCore <$ UI.click step)

    getBody w #+ [column $ map element [step, coreTab]]

    onEvent (stepBehaviour <@ UI.click step) $ \m -> do
        (mars, taskList) <- liftIO $ takeStep ctxt count
        element coreTab # set children []
        element coreTab #+ (tableHeader : (tableContents mars taskList))
        return ()

    return ()
\end{code}

The step function sends a signal on the synchronisation channel for all workers to execute a single step. It then collects the list of program counters returned on the task channel and aggregates them with their labels into a list to be consumed by the table producing functions.

\begin{code}
takeStep :: (TChan Bool, MVar Mars, TChan (Char, [Int])) -> Int -> IO (Mars, [(Char, [Int])])
takeStep (sigChan, mCore, taskChan) taskCount = do
    atomically $ writeTChan sigChan True
    taskMap <- getChanMap taskCount taskChan
    core <- readMVar mCore
    return (core, Map.assocs taskMap)

getChanMap :: Int -> TChan (Char, [Int]) -> IO (Map.Map Char [Int])
getChanMap = fillMap Map.empty

fillMap :: Map.Map Char [Int] -> Int -> TChan (Char, [Int]) -> IO (Map.Map Char [Int])
fillMap m 0 _ = return m
fillMap m x tchan = do
    (label, tasks) <- atomically $ readTChan tchan
    m' <- fillMap m (x - 1) tchan
    return $ Map.insert label tasks m'
\end{code}

The table consists of a header labelling the columns in the table, along with a listing of all the instructions currently in the core. This is done by retrieving the instructions from the core and aggregating the task messages into that listing.

I couldn't get threepenny to use the CSS file I gave it despite trying a few different ways, so instead the instructions are given inline styles.

\begin{code}
tableHeader :: UI Element
tableHeader = UI.tr #+ map (toTextElement UI.th) ["Tasks", "Position", "Instruction"]

tableContents :: Mars -> [(Char, [Int])] -> [UI Element]
tableContents mars taskList = map (\[pos, tasks, ins] -> UI.tr #+ [tasks, pos, ins # set UI.style [("font-family", "monospace")]]) elements
    where instructions = display mars
          insMap = Map.fromList $ map (\(pos, ins) -> (pos, ("", ins))) instructions
          taskMap = foldl (\m (label, tasks) -> addTasks label m tasks) insMap taskList
          rowAssocs = Map.assocs taskMap
          elements = map (\(pos, (tasks, ins)) -> map (toTextElement UI.td) [sort tasks, show pos, ins]) rowAssocs

addTasks :: Char -> Map.Map Int (String, String) -> [Int] -> Map.Map Int (String, String)
addTasks label = foldl (\m task -> Map.insertWith (\_ (labels, ins) -> (label : labels, ins)) task ([label], show defaultIns) m)

toTextElement :: UI Element -> String -> UI Element
toTextElement eType = (\x -> eType # set text x)
\end{code}
