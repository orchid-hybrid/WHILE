module Compile where

import Syntax
import X86

import Control.Monad.State

--compile :: S -> [Instruction]
compileS e = evalState (compile' e) ([], identifiers) where
    compile' :: S -> State ([String], [String]) (Operand, [Instruction])
    compile' SKIP = do
        i <- pop
        push i
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
        return (r_n,  c' ++ [
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
        push counter
        start <- pop
        end <- pop
        return (r_p,  c' ++ [
                      MOV (Label_R counter) (Immediate (Concrete 0)),
                      Label start,
                      CMP r_c (Immediate (Concrete 1)),
                      JE (Label_R end)
                      ] ++ p' ++ [
                      ADD r_c (Immediate (Concrete 1)),
                      JMP (Label_R start),
                      Label end])

compileB :: B -> State ([String], [String]) (Operand, [Instruction])
compileB BTrue = do
    r <- pop
    return (Label_R r, [MOV (Label_R r) (Immediate (Concrete 1))])
compileB BFalse = do
    r <- pop
    return (Label_R r, [MOV (Label_R r) (Immediate (Concrete 0))])
compileB (BNot b) = do
    (r, b') <- compileB b
    false <- pop
    end <- pop
    return (r, (b' ++
           [CMP r (Immediate (Concrete 0)),
            JNE (Label_R false),
            MOV r (Immediate (Concrete 1)),
            JMP (Label_R end),
            Label false,
            MOV r (Immediate (Concrete 0)),
            Label end]))
compileB (BOpB o p q) = do
    (r_p, p') <- compileB p
    (r_q, q') <- compileB q

    case o of
        And -> return (r_q, p' ++
                            q' ++
                           [XOR r_p r_q,
                            CMP r_p)
        Or  ->

pop = do
    (ls, (x:xs)) <- get
    put (ls, xs)
    return x

push l = do
    (ls, ids) <- get
    put (l:ls, ids)

identifiers = (map return ['a'..'z'] :: [String]) ++ (map (\n -> 'i':(show n)) [1..])
