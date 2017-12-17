\begin{code}
module Instruction.Instruction
    ( Instruction(..), defaultIns, aField, bField, aValue, bValue, withA, withB,
      Value(..), valuePart, swapValue
    ) where

import Data.Char (isDigit, toLower, isSpace)
\end{code}

Instructions are composed of an opcode and up to two fields, A and B, each containing a value. These values have their own semantics. In instructions where only one field is used, the other field is zero.

We define a few convenience functions to access the value fields, since the variable number of fields doesn't lend itself to the record syntax.

\begin{code}
data Instruction = Dat Value
                 | Mov Value Value
                 | Add Value Value
                 | Sub Value Value
                 | Jmp Value
                 | Jmz Value Value
                 | Jmn Value Value
                 | Djn Value Value
                 | Cmp Value Value
                 | Spl Value

defaultIns :: Instruction
defaultIns = Dat (Direct 0)

aField :: Instruction -> Value
aField ins = case ins of
                Dat _   -> defaultVal
                Mov v _ -> v
                Add v _ -> v
                Sub v _ -> v
                Jmp v   -> v
                Jmz v _ -> v
                Jmn v _ -> v
                Djn v _ -> v
                Cmp v _ -> v
                Spl v   -> v

aValue :: Instruction -> Int
aValue = valuePart . aField

bField :: Instruction -> Value
bField ins = case ins of
                Dat v   -> v 
                Mov _ v -> v
                Add _ v -> v
                Sub _ v -> v
                Jmp _   -> defaultVal
                Jmz _ v -> v
                Jmn _ v -> v
                Djn _ v -> v
                Cmp _ v -> v
                Spl _   -> defaultVal

bValue :: Instruction -> Int
bValue = valuePart . bField

withA :: Instruction -> Int -> Instruction
withA ins i = case ins of
    Dat b   -> Dat b
    Mov a b -> Mov (swap a) b
    Add a b -> Add (swap a) b
    Sub a b -> Sub (swap a) b
    Jmp a   -> Jmp (swap a)
    Jmz a b -> Jmz (swap a) b
    Jmn a b -> Jmn (swap a) b
    Djn a b -> Djn (swap a) b
    Cmp a b -> Cmp (swap a) b
    Spl a   -> Spl (swap a)
    where swap = swapValue i

withB :: Instruction -> Int -> Instruction
withB ins i = case ins of
    Dat b   -> Dat (swap b)
    Mov a b -> Mov a (swap b)
    Add a b -> Add a (swap b)
    Sub a b -> Sub a (swap b)
    Jmp a   -> Jmp a
    Jmz a b -> Jmz a (swap b)
    Jmn a b -> Jmn a (swap b)
    Djn a b -> Djn a (swap b)
    Cmp a b -> Cmp a (swap b)
    Spl a   -> Spl a
    where swap = swapValue i
\end{code}

Values are comprised of two parts, the value and the addressing mode. The addressing mode may be ignored depending on the instruction.

\begin{code}
data Value = Direct Int
           | Indirect Int
           | Immediate Int
           | Autodecrement Int

defaultVal :: Value
defaultVal = Direct 0
\end{code}

We also define functions to swap out the value part, useful for updating instructions as a result of a ADD, SUB, DJN, or resulotion of an Autodecrement address.

\begin{code}
valuePart :: Value -> Int
valuePart (Direct a) = a
valuePart (Indirect a) = a
valuePart (Immediate a) = a
valuePart (Autodecrement a) = a

swapValue :: Int -> Value -> Value
swapValue i (Direct _) = Direct i
swapValue i (Indirect _) = Indirect i
swapValue i (Immediate _) = Immediate i
swapValue i (Autodecrement _) = Autodecrement i
\end{code}

We want to be able to parse redcode programs, so we'll need a custom Read instance for Instruction, which will require one for Value as well. Since Value is the simpler one (and we'll need it to parse the Instruction anyway), we'll start with that. (with thanks to this StackOverflow answer for showing a clear and simple implementation of Read https://stackoverflow.com/a/14006938/6519610)

\begin{code}
instance Read Value where
    readsPrec _ input = 
        let (mode, rest1) = span (flip elem "$@#<") input
            (num, rest2) = span (\a -> isDigit a || a == '-') rest1
            value = read num :: Int
            addressing = case mode of
                         ""        -> Direct
                         "$"       -> Direct
                         "@"       -> Indirect
                         "#"       -> Immediate
                         "<"       -> Autodecrement
            in [(addressing value, rest2)]
\end{code}

Now we just need to do the same for the Instruction. This part is slightly more complicated since we need to check if the constructor needs one or two values. We split the parsing into two sub functions - one which parses an instruction with a single value and one which parses instructions with two values.

We will ignore the existence of comments, assume lines are not blank, and assume that lines do not contain only comments. These cases will all be handled by the parser/pre-processor.

\begin{code}
instance Read Instruction where
    readsPrec _ i = parseWith op rest2
      where input = map toLower i
            (op, rest1) = span (not . isSpace) input
            rest2 = dropWhile isSpace rest1
            parseWith = if op `elem` ["dat", "jmp", "spl"] then readSingle else readDouble

readSingle :: String -> String -> [(Instruction, String)]
readSingle op input = 
    let constructor = case op of
                        "dat" -> Dat
                        "jmp" -> Jmp
                        "spl" -> Spl
        [(value, rest1)] = readsPrec 0 input :: [(Value, String)]
    in [(constructor value, rest1)]

readDouble :: String -> String -> [(Instruction, String)]
readDouble op input =
    let constructor = case op of
                        "mov" -> Mov
                        "add" -> Add
                        "sub" -> Sub
                        "jmz" -> Jmz
                        "jmn" -> Jmn
                        "djn" -> Djn
                        "cmp" -> Cmp
                        otherwise -> error $ "Invalid opcode " ++ op
        [(first, rest1)] = readsPrec 0 input :: [(Value, String)]
        rest2 = dropWhile isSpace $ dropIf ',' $ dropWhile isSpace rest1
        [(second, rest3)] = readsPrec 0 rest2 :: [(Value, String)]
    in [(constructor first second, rest3)]
    where dropIf c xs = if (head xs == c) then drop 1 xs else error $ "Expected " ++ [c] ++ " between values but got " ++ [head xs]
\end{code}

This concludes the definition of Instructions, with the effects of their execution defined elsewhere in the model to actually assign semantics to their execution.

However, since we've defined read, and we'd like `read . show` to be equivalent to `id`, we'll define show for Value and Instruction, rather than deriving it. This will also be useful elsewhere for pretty printing.

\begin{code}
instance Show Value where
    show (Direct a) = "$" ++ (show a)
    show (Indirect a) = "@" ++ (show a)
    show (Immediate a) = "#" ++ (show a)
    show (Autodecrement a) = "<" ++ (show a)

instance Show Instruction where
    show (Dat v1)        = "DAT " ++ (show v1)
    show ins@(Mov v1 v2) = show2 "MOV" ins
    show ins@(Add v1 v2) = show2 "ADD" ins
    show ins@(Sub v1 v2) = show2 "SUB" ins
    show (Jmp v1)        = "JMP " ++ (show v1)
    show ins@(Jmz v1 v2) = show2 "JMZ" ins
    show ins@(Jmn v1 v2) = show2 "JMN" ins
    show ins@(Djn v1 v2) = show2 "DJN" ins
    show ins@(Cmp v1 v2) = show2 "CMP" ins
    show (Spl v1)        = "SPL " ++ (show v1)

show2 :: String -> Instruction -> String
show2 op ins = op ++ " " ++ (show $ aField ins) ++ ", " ++ (show $ bField ins)
\end{code}
