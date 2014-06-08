{-# LANGUAGE GADTs #-}

module Main where

import           Control.Monad.Operational

main :: IO ()
main = print "primo"

type Stack a = [a]

-- Credo che il primo passo da notare sia che se vogliamo ritornare
-- qualcosa mettiamo un tag corrispondente nell'espressione

-- Questo e' come dire StackInstruction = Pop | Push Int
-- piu' le annotazioni di ritorno, che sono una specializzazione del
-- dato che ci dice cosa stiamo ritornando esattamente.
data StackInstruction a where
    Pop  :: StackInstruction Int
    Push :: Int -> StackInstruction ()

-- A questo punto implementiamo il tipo programma, parametrizzato
-- sull'istruzione e sul tipo di valore. Questo in effetti simula
-- il comportamento del bind!
-- Il significato del bind e': se ti do un'istruzione che ritorna a
-- e una funzione da a ad un programma con istruzioni che ritorna b
-- allora tu mi sai dare un programma che ritorna b
-- Notiamo che l'istruzione return denota un'istruzione semplice che
-- ha come unico scopo quello di ritornare il valore che le viene passato

--Odata Program instr a where
--O    Then :: instr a -> (a -> Program instr b) -> Program instr b
--O    Return :: a -> Program instr a

type StackProgram a = Program StackInstruction a

-- A questo punto ecco la funzione di interpretazione

--Ointerpret :: StackProgram a -> Stack Int -> a
--Ointerpret (Push a `Then` is) stack     = interpret (is ()) (a:stack)
--Ointerpret (Pop    `Then` is) (b:stack) = interpret (is b) stack
--Ointerpret (Return c)         _         = c

interpret :: StackProgram a -> Stack Int -> a
interpret = eval . view
    where
      eval :: ProgramView StackInstruction a -> Stack Int -> a
      eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
      eval (Pop    :>>= is) (b:stack) = interpret (is b) stack
      eval (Return c)       _         = c


-- Il punto di tutta questa storia e' che Push a ha tipo StackInstruction ()
-- modellando il fatto che non ritorna niente nell'esecuzione del programma

--Oexample :: StackProgram Int
--Oexample = Pop `Then` \a -> Pop `Then` \b -> Return (a*b)

--Osimple :: StackProgram Int
--Osimple = Pop `Then` \a -> Return a

-- Ora il problema e' sfocare la distinzione tra istruzioni semplici
-- e programmi, che e' indipendente dall'interprete. E' proprio questa
-- indipendenza dall'interprete che lo rende equivalente all'avvolgere
-- un solo elemento in una lista.

--Osingleton :: instr a -> Program instr a
--Osingleton i = i `Then` Return

pop :: StackProgram Int
pop = singleton Pop

push :: Int -> StackProgram ()
push = singleton . Push

-- infine, il layer monadico, che serve in effetti
-- per concatenare delle strutture simil-lista
--Oinstance Monad (Program instr) where
--O    (Return a) >>= js = js a
--O    (i `Then` is) >>= js = i `Then` (is >=> js)
--O    return = Return

complex :: StackProgram Int
complex = do
  a <- pop
  b <- pop
  c <- pop
  return (a*b+c)
