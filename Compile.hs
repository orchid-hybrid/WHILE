module Compile where

import Syntax
import X86

import Control.Monad.State

--compile :: S -> [Instruction]
compile e = evalState (compile' e) ([], identifiers) where
    compile' :: S -> State ([String], [String]) (Operand, [Instruction])
    compile' SKIP = do
        i <- pop
        return (Label_R i, [NOP])
    compile' (p :>> q) = do
        (_, p') <- compile' p
        (r, q') <- compile' q
        return (r, p' ++ q')
    compile' (IF c e n) = do
        (r_c, c') <- compileB c
        (r_e, e') <- compile' e
        (r_n, n') <- compile' n
        n_l       <- pop
        return (r_n, [c',
                      CMP r_c (Immediate (Concrete 1)),
                      JNE (Label_R n_l)
                      ] ++ e' ++ [
                      MOV r_e r_n,
                      Label n_l
                      ] ++ n')
    compile' (WHILE c p) = do
        (r_c, c') <- compileB c
        (r_p, p') <- compile' p
        counter <- pop
        start <- pop
        end <- pop
        return (r_p, [c',
                      Label start,
                      CMP r_c (Immediate (Concrete 1)),
                      JE (Label_R end)
                      ] ++ p' ++ [
                      ADD r_c (Immediate (Concrete 1)),
                      JMP (Label_R start),
                      Label end])
        

    pop = do
        (ls, (x:xs)) <- get
        put (ls, xs)
        return x

    identifiers = (map return ['a'..'z'] :: [String]) ++ (map (\n -> 'i':(show n)) [1..])

compileB = undefined
