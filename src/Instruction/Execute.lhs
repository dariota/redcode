\begin{code}
module Instruction.Execute (
    execute
) where

import Prelude hiding (lookup)

import Instruction.Resolver
import Control.Monad.Trans.State (get)
import Instruction.Instruction
import Core
\end{code}

This module will define only the semantics of instruction execution. That is, given an instruction it will determine what effect executing it would have (adjusting the program counter, creating new instructions etc).

These functions execute in the context of the Core's state monad, which makes tracking its state changes as references are resolved and commands executed easy. We simply take in a position to execute a command at and perform all the required actions. We then return a list indicating the new program counters that replace the task that was just executed.

Using a list here allows the handling of SPL to be very similar to the other instructions, and allows new instructions to spawn any number of child tasks.

\begin{code}
type PcUpdate = [Int]

execute :: Int -> Core PcUpdate
execute p = do
    ins <- lookup p
    newPcs <- executeIns p ins
    mars <- get
    pure $ map (adjustWith mars) newPcs

executeIns :: Int -> Instruction -> Core PcUpdate
executeIns pos ins = case ins of
    -- terminate execution - the PC is gone
    Dat _     -> pure []
    -- More complicated, see below
    Mov _  _  -> do
        executeMov pos ins
        pure defU
    -- Add the A value to whatever the B field resolves to
    Add _ _  -> do
        bRef <- resolve pos $ bField ins
        executeArith bRef $ aValue ins
        pure defU
    -- Same as Add, but with a negative delta
    Sub _  _  -> do
        bRef <- resolve pos $ bField ins
        executeArith bRef $ -(aValue ins)
        pure defU
    -- Jump to whatever the A field resolves to
    Jmp _     -> do
        aRef <- resolve pos (aField ins)
        pure [aRef]
    -- Jump to whatever the A field resolves to if the B field resolves to 0
    Jmz _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        if bRef == 0 then
            pure [aRef]
        else
            pure defU
    -- Like Jmz, but jumps if the B field doesn't resolve to 0
    Jmn _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        if bRef /= 0 then
            pure [aRef]
        else
            pure defU
    -- Like Jmn, but decrements what the B field resolves to first
    Djn _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        decResult <- decrementAt bRef
        if decResult /= 0 then
            pure [aRef]
        else
            pure defU
    -- Skips an instruction if the fields resolve to the same thing
    Cmp _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        if aRef == bRef then
            pure [2 + pos]
        else
            pure defU
    -- Creates a new task wherever the A field resolves to
    Spl _     -> do
        aRef <- resolve pos $ aField ins
        pure [aRef, defI]
    where defI = 1 + pos
          defU = [defI]
\end{code}

Pretty much every instruction needs both of its fields resolved, so we define a helper function to handle this, ensuring they're always resolved in the same order since field resolution can have side effects (Autodecrement values).

\begin{code}
resolveRefs :: Int -> Instruction -> Core (Int, Int)
resolveRefs pos ins = do
    aRef <- resolve pos (aField ins)
    bRef <- resolve pos (bField ins)
    pure (aRef, bRef)
\end{code}

Since a few instructions need to perform arithmetic, we define a helper function to handle that for us too.

\begin{code}
executeArith :: Int -> Int -> Core ()
executeArith insPos delta = do
    instruction <- lookup insPos
    let val = valuePart $ bField instruction
    let updated = withB instruction (val + delta)
    insert insPos updated

decrementAt :: Int -> Core Int
decrementAt i = do
    executeArith i (-1)
    refIns <- lookup i
    pure $ valuePart $ bField refIns
\end{code}

Note that for the Jmz/Jmn instruction, it has been assumed that "doing nothing" means taking the default behaviour of moving 1 step forward.

The mov instruction is separately implemented due to some complexity around how it treats its two fields.

\begin{code}
executeMov :: Int -> Instruction -> Core ()
executeMov _ (Mov _ (Immediate _)) = error "MOV B Field must not be immediate"
executeMov i (Mov (Immediate a) vb) = do
    bRef <- resolve i vb
    insert bRef (Dat (Direct a))
executeMov i ins = do
    (aRef, bRef) <- resolveRefs i ins
    toCopy <- lookup aRef
    insert bRef toCopy
\end{code}
