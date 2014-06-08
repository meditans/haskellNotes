{-# LANGUAGE GADTs #-}

module Main where

import           Control.Monad.Operational

main :: IO ()
main = print "primo"

type Stack a = [a]

data StackInstruction a where
    Pop  :: StackInstruction Int
    Push :: Int -> StackInstruction ()

type StackProgram a = Program StackInstruction a

pop :: StackProgram Int
pop = singleton Pop

push :: Int -> StackProgram ()
push = singleton . Push

interpret :: StackProgram a -> Stack Int -> a
interpret = eval . view
    where
      eval :: ProgramView StackInstruction a -> Stack Int -> a
      eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
      eval (Pop    :>>= is) (b:stack) = interpret (is b) stack
      eval (Return c)       _         = c

interpretStory :: StackProgram a -> Stack Int -> [Stack Int]
interpretStory = eval . view
    where
      eval :: ProgramView StackInstruction a -> Stack Int -> [Stack Int]
      eval (Push a :>>= is) stack     = stack     : interpretStory (is ()) (a:stack)
      eval (Pop    :>>= is) (b:stack) = (b:stack) : interpretStory (is b) stack
      eval (Return _)       final     = [final]

complex :: StackProgram Int
complex = do
  a <- pop
  b <- pop
  c <- pop
  push (b*c)
  return (a+b)
