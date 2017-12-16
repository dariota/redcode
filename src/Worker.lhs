\begin{code}
module Worker (
    worker
) where

import Core
import Executor
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar
import Control.Monad.Trans.State
\end{code}

We want to be able to control the simulation stepping externally, so that we can limit the number of steps, or the speed at which the steps execute, and also indicate when to terminate.

We manage this by having a TChan with a Bool for every step to be taken. This allows a discrete number of steps to be taken (by filling the channel with that number of True, followed by a False), while also allowing the channel writer to delay the next step (by not adding a True until the next step should be taken).

\begin{code}
type Stepper = TChan Bool

takeStep :: Stepper -> IO Bool
takeStep s = atomically $ readTChan s
\end{code}

Next, we want the worker to be able to signal to a listener how many tasks it has running, using some label for itself (from the spec, we'll use a Char as the label, for A, B, etc.).

\begin{code}
type TaskLabel = Char
type TaskChan = TChan (TaskLabel, [Int])

signalTask :: TaskChan -> TaskLabel -> [Int] -> IO ()
signalTask tchan label positions = atomically $ writeTChan tchan (label, positions)
\end{code}

A worker must be able to determine when to take a step (using the Stepper), access the shared core, simulate a step of its executors, and signal to a listener how many tasks remain after each step (using the TaskChan above).

The core will be shared using an MVar, as only one executor can act on the core at a time anyway. Workers will retrieve the core, execute a single step using their executor if the simulation is still running, report the task count, and repeat.

\begin{code}
worker :: Stepper -> MVar (Mars) -> TaskChan -> TaskLabel -> Executor -> IO ()
worker stepper mcore tchan label exec = do
    continue <- takeStep stepper
    if continue then
        do
            core <- takeMVar mcore
            let (exec', core') = runState (step exec) core
            putMVar mcore core'
            signalTask tchan label $ tasks exec'
            worker stepper mcore tchan label exec'
    else
        return ()
\end{code}
