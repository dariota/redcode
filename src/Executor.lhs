\begin{code}
module Executor (
    Executor, executor, step, size
) where

import Core
import Instruction.Execute
import Data.Maybe (fromJust)
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

We want to pass the core to the executor and have it execute a single step (with its next task), then return the modified core and new executor, or nothing if all tasks have ended.

\begin{code}
step :: Executor -> Core -> (Executor, Core)
step e@(Executor []) c     = (e, c)
step (Executor (t:ts)) c = (mExec, nC)
    where ((nT, child), nC) = execute t c
          childTail = if child == Nothing then [] else [fromJust child]
          mExec = if nT == Nothing then Executor ts else Executor $ ts ++ childTail ++ [fromJust nT]
\end{code}

In order to actually execute this, we now need to put it in a thread that repeatedly calls step while the simulation is running, managing the sharing of the core and cleanup of the executor when it terminates.
