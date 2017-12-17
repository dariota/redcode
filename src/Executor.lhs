\begin{code}
module Executor (
    Executor, executor, step, size, tasks
) where

import Core
import Instruction.Execute
import Data.Maybe (isNothing, fromJust)
\end{code}

An execution unit (or program) in redcode is a collection of tasks. A task is simply a program counter - its execution can alter the state of the core, alter its program counter, or prevent its future execution.

We want to be able to specify where an executor should start its execution, so we define a function for this as well.

\begin{code}
data Executor = Executor {tasks :: [Int]}

executor :: Int -> Executor
executor start = Executor [start]

size :: Executor -> Int
size (Executor xs) = length xs
\end{code}

We want to pass the core to the executor and have it execute a single step (with its next task), updating the core and returning an updated executor with all the task PCs resulting from the execution.

\begin{code}
step :: Executor -> Core Executor
step e@(Executor [])   = pure e
step (Executor (t:ts)) = do
    newPcs <- execute t
    pure $ Executor $ ts ++ newPcs
\end{code}

In order to actually execute this, we now need to put it in a thread that repeatedly calls step while the simulation is running, which will manage the sharing of the core.
