module While (
 compile
 ) where

-- https://www.youtube.com/watch?v=PbL9vr4Q2LU

import Control.Monad.Trans
import Control.Monad.Writer
import Control.Applicative
import Data.Monoid
import Data.List
import Data.Int

import Fresh
import Syntax
import Assembly

type Compile a = WriterT [Asm Bool] (Fresh String) a

compileA :: A -> It Bool -> Compile ()
compileA (V s) r@(Register _) = tell [AMov r (Cell s)]
compileA (V s) r = tell [AMov (Register True) (Cell s), AMov r (Register True)]
compileA (N i) r = tell [AMov r (Num . fromIntegral $ i)]
compileA (AOpA op x y) r = do
 a <- lift $ fresh
 compileA y (Cell a)
 compileA y (Register True)
 case op of
  Plus -> tell [AAdd (Register True) (Cell a)]
  Minus -> tell [ASub (Register True) (Cell a)]
  Times -> tell [AMul (Cell a)]
 if r == Register True
    then return ()
    else tell [AMov r (Register True)]

-- <Jester01> actually you can do "or rax, -1"
-- <Jester01> and that will do your true
-- <Jester01> including clearing the ZF

-- SHOULD USE BL, not RBX for booleans...
-- True = RAX
-- False = BL

compileOpR Equal = CEqual
compileOpR Less = CLess
compileOpR LessEq = CLessEq
compileOpR Greater = CGreater
compileOpR GreaterEq = CGreaterEq

compileOppositeOpR Equal = CNotEqual
compileOppositeOpR NotEqual = CEqual
compileOppositeOpR Less = CGreaterEq
compileOppositeOpR LessEq = CGreater
compileOppositeOpR Greater = CLessEq
compileOppositeOpR GreaterEq = CLess

compileB :: B -> Compile ()
compileB BTrue = tell [AOr (Register False) (Num (-1))]
compileB BFalse = tell [AXor (Register False) (Register False)]
compileB (BNot b) = do compileB b ; tell [ANot (Register False), ATest (Register False) (Register False)]
compileB (BOpB And p q) = do
 lbl <- lift $ ("l"++) <$> fresh
 compileB p
 tell [ AJmp (Just CZero) lbl ]
 compileB q
 tell [ALabel lbl]
compileB (BOpB Or p q) = do
 lbl <- lift $ ("l"++) <$> fresh
 compileB p
 tell [ AJmp (Just CNotZero) lbl ]
 compileB q
 tell [ALabel lbl]
compileB (BOpR op x y) = do
 a <- lift $ fresh
 compileA x (Cell a)
 compileA y (Register True)
 tell [ case op of
         Equal -> ATest (Cell a) (Register True)
         _     -> ACmp  (Cell a) (Register True) 
      , ASetByte (compileOppositeOpR op) (Register False)
      , ADec (Register False)
      ]

compileS :: S -> Compile ()
compileS (v := a) = compileA a (Cell v)
compileS SKIP = return ()
compileS (m :>> n) = compileS m >> compileS n
compileS (IF b t e) = do
 lbl1 <- lift $ ("l"++) <$> fresh
 lbl2 <- lift $ ("l"++) <$> fresh
 compileB b
 tell [AJmp (Just CZero) lbl1]
 compileS t
 tell [AJmp Nothing lbl2]
 tell [ALabel lbl1]
 compileS e
 tell [ALabel lbl2]
compileS (WHILE c b) = do
 lbl1 <- lift $ ("l"++) <$> fresh
 lbl2 <- lift $ ("l"++) <$> fresh
 tell [ALabel lbl1]
 compileB c
 tell [AJmp (Just CZero) lbl2]
 compileS b
 tell [AJmp Nothing lbl1]
 tell [ALabel lbl2]

runCompiler m = snd $ runFresh (runWriterT m) (map (("c"++) . show) [0..])

compile = runCompiler . compileS

{-

aop Add = AAdd
aop Sub = ASub

runElla exp = snd $ runFresh (runWriterT $ ella exp (Register ())) [0..]

-}
