\begin{code}
module Instruction.Execute (
    execute
) where

import Prelude hiding (lookup)

import Instruction.Resolver
import Instruction.Instruction
import Core
\end{code}

This module will define only the semantics of instruction execution. That is, given an instruction it will determine what effect executing it would have (adjusting the program counter, creating new instructions etc).

These functions execute in the context of the Core's state monad, which makes tracking its state changes as references are resolved and commands executed easy. We simply take in a position to execute a command at and perform all the required actions. We then return a tuple indicating the program counter after execution of the specified command.

However, we also need to cater for the SPL instruction, which can result in a new task being created. Hence, we return two program counter updates - the first for the current task, the second for the creation of a new task if relevant. If we wanted to generalise to the possibility of creating n tasks per instruction executed, we could simply replace the tuple with a list.

\begin{code}
type PcUpdate = Maybe Int

execute :: Int -> Core (PcUpdate, PcUpdate)
execute p = do
    ins <- lookup p
    (task, child) <- executeIns p ins
    adjTask <- adjustM task
    adjChild <- adjustM child
    pure (adjTask, adjChild)

-- Should've used MaybeT, or a list...
adjustM :: Maybe Int -> Core (Maybe Int)
adjustM Nothing = pure Nothing
adjustM (Just x) = do
    adjX <- adjust x
    pure $ Just adjX

executeIns :: Int -> Instruction -> Core (PcUpdate, PcUpdate)
executeIns pos ins = case ins of
    -- terminate execution - the PC becomes Nothing
    Dat _     -> pure $ (Nothing, Nothing)
    -- More complicated, see below
    Mov _  _  -> do
        executeMov pos ins
        pure $ defU
    -- Add the A value to whatever the B field resolves to
    Add _ _  -> do
        bRef <- resolve pos $ bField ins
        executeArith bRef $ aValue ins
        pure $ defU
    -- Same as Add, but with a negative delta
    Sub _  _  -> do
        bRef <- resolve pos $ bField ins
        executeArith bRef $ -(aValue ins)
        pure defU
    -- Jump to whatever the A field resolves to
    Jmp _     -> do
        aRef <- resolve pos (aField ins)
        pure $ (Just aRef, Nothing)
    -- Jump to whatever the A field resolves to if the B field resolves to 0
    Jmz _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        if bRef == 0 then
            pure (Just aRef, Nothing)
        else
            pure defU
    -- Like Jmz, but jumps if the B field doesn't resolve to 0
    Jmn _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        if bRef /= 0 then
            pure (Just aRef, Nothing)
        else
            pure defU
    -- Like Jmn, but decrements what the B field resolves to first
    Djn _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        decResult <- decrementAt bRef
        if decResult /= 0 then
            pure (Just aRef, Nothing)
        else
            pure defU
    -- Skips an instruction if the fields resolve to the same thing
    Cmp _  _  -> do
        (aRef, bRef) <- resolveRefs pos ins
        if aRef == bRef then
            pure (Just (2 + pos), Nothing)
        else
            pure defU
    -- Creates a new task wherever the A field resolves to
    Spl _     -> do
        aRef <- resolve pos $ aField ins
        pure (defI, Just aRef)
    where defI = Just (1 + pos)
          defU = (defI, Nothing)
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
