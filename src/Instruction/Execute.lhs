\begin{code}
module Instruction.Execute (
    execute, executeArith, decrementAt
) where

import Prelude hiding (lookup)

import Instruction.Resolver
import Instruction.Instruction
import Core
\end{code}

This module will define only the semantics of instruction execution. That is, given an instruction it will determine what effect executing it would have (adjusting the program counter, creating new instructions etc).

We need access to the memory in order to execute instructions, so we take in a position and the core, and return an update to the program counter (or nothing, if execution of the instruction would cause termination) as well as the core after execution of the instruction.

However, we also need to cater for the SPL instruction, which can result in a new task being created. Hence, we return two program counter updates - the first for the current task, the second for the creation of a new task if relevant.

\begin{code}
type PcUpdate = Maybe Int

execute :: Int -> Core -> ((PcUpdate, PcUpdate), Core)
execute p c = executeIns p (lookup c p) c

executeIns :: Int -> Instruction -> Core -> ((PcUpdate, PcUpdate), Core)
executeIns pos ins c = case ins of
    Dat _     -> ((Nothing, Nothing), c)
    Add v1 _  -> (defU, insert cB resolvedB $ executeArith bIns $ valuePart v1)
    Sub v1 _  -> (defU, insert cB resolvedB $ executeArith bIns $ -(valuePart v1))
    Jmp _     -> ((Just resolvedA, Nothing), cA)
    Jmz _  _  -> (if resolvedB == 0 then (Just resolvedA, Nothing) else defU, cB)
    Jmn _  _  -> (if resolvedB /= 0 then (Just resolvedA, Nothing) else defU, cB)
    Djn _  _  -> (if decResult /= 0 then (Just resolvedA, Nothing) else defU, cDjn)
    Cmp _  _  -> (if resolvedA == resolvedB then defU else (Just 2, Nothing), cAB')
    Spl _     -> ((Just 1, Just resolvedA), cA)
    where aFie = aField ins
          bFie = bField ins
          (resolvedA, cA) = resolve pos aFie c
          (resolvedB, cB) = resolve pos bFie c
          (resolvedA', cAB) = resolve pos aFie c
          (resolvedB', cAB') = resolve pos bFie c
          bIns = lookup c resolvedB
          defU = (Just 1, Nothing)
          (decResult, cDjn) = decrementAt resolvedB cAB'

executeArith :: Instruction -> Int -> Instruction
executeArith ins i = withB ins (insB + i)
    where insB = valuePart $ bField ins

decrementAt :: Int -> Core -> (Int, Core)
decrementAt i c = (bVal, insert c i ins')
    where ins = lookup c i
          ins' = executeArith ins (-1)
          bVal = valuePart $ bField ins'
\end{code}

Note that for the Jmz/Jmn instruction, it has been assumed that "doing nothing" means taking the default behaviour of moving 1 step forward.

Also, this is a clear absolute mess, and the core should probably have instead been a monad in which these instructions execute - unfortunately between this and other assignments I don't have the time to redesign this to fix it at the moment :(
