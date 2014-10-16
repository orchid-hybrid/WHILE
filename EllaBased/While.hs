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

data Regs = RAX | BL
 deriving (Eq, Show)

type Compile a = WriterT [Asm Regs] (Fresh String) a

compileA :: A -> It Regs -> Compile ()
compileA (V s) r@(Register _) = tell [AMov r (Cell s)]
compileA (V s) r = tell [AMov (Register RAX) (Cell s), AMov r (Register RAX)]
compileA (N i) r = tell [AMov r (Num . fromIntegral $ i)]
compileA (AOpA op x y) r = do
 a <- lift $ fresh
 compileA y (Cell a)
 compileA x (Register RAX)
 case op of
  Plus -> tell [AAdd (Register RAX) (Cell a)]
  Minus -> tell [ASub (Register RAX) (Cell a)]
  Times -> tell [AMul (Cell a)]
 if r == Register RAX
    then return ()
    else tell [AMov r (Register RAX)]

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
compileB BTrue = tell [AOr (Register BL) (Num (-1))]
compileB BFalse = tell [AXor (Register BL) (Register BL)]
compileB (BNot b) = do compileB b ; tell [ANot (Register BL), ATest (Register BL) (Register BL)]
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
 compileA y (Register RAX)
 tell [ case op of
         Equal -> ACmp  (Cell a) (Register RAX)
               --ATest (Cell a) (Register RAX)
               -- test does not work
         _     -> ACmp  (Cell a) (Register RAX) 
      , ASetByte (compileOppositeOpR op) BL
      , ADec (Register BL)
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
