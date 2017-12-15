\begin{code}
module Executor (
    Executor, executor, step, size
) where

import Core
import Instruction.Execute
import Data.Maybe (isNothing, fromJust)
\end{code}

An execution unit (or program) in redcode is a collection of tasks. A task is simply a program counter - its execution can alter the state of the core, alter its program counter, or prevent its future execution.

We want to be able to specify where an executor should start its execution, so we define a function for this as well.

\begin{code}
data Executor = Executor [Int]

executor :: Int -> Executor
executor start = Executor [start]

size :: Executor -> Int
size (Executor xs) = length xs
\end{code}

We want to pass the core to the executor and have it execute a single step (with its next task), then return the new executor with updated child tasks.

While we know that the childPc will never exist without the task getting assigned a new PC, we'll assume minimal knowledge and support one without the other.

\begin{code}
step :: Executor -> Core Executor
step e@(Executor [])   = pure e
step (Executor (t:ts)) = do
    (newPc, childPc) <- execute t
    let childTail = if isNothing childPc then [] else [fromJust childPc]
    let taskQueue = if isNothing newPc then ts ++ childTail else ts ++ childTail ++ [fromJust newPc]
    pure $ Executor taskQueue
\end{code}

In order to actually execute this, we now need to put it in a thread that repeatedly calls step while the simulation is running, managing the sharing of the core and cleanup of the executor when it terminates.
