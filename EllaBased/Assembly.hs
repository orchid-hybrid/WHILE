{-# LANGUAGE NoMonomorphismRestriction #-}

module Assembly (
 It(..), Condition(..), Asm(..),
 s
 ) where

import Data.Int
import Data.Char

data It reg
 = Register reg
 | Deref reg
 | Cell String
 | Num Int32
 deriving (Eq, Show)

data Condition
 = CEqual
 | CNotEqual
 | CLess
 | CLessEq
 | CGreater
 | CGreaterEq
 
 | CZero
 | CNotZero
 deriving (Eq, Show)

data Asm reg
 = AAdd (It reg) (It reg)
 | ASub (It reg) (It reg)
 | AMul (It reg)
 | ADec (It reg)
 
 | ANot (It reg)
 | AAnd (It reg) (It reg)
 | AOr (It reg) (It reg)
 | AXor (It reg) (It reg)
 | ASetByte Condition reg
 
 | ACmp (It reg) (It reg)
 | ATest (It reg) (It reg)
 
 | ALabel String
 | AJmp (Maybe Condition) String
 | ACompJmp reg
 | AMov (It reg) (It reg)
 deriving (Eq, Show)

---------------

red = map toLower . show

op (Register r) = red r
op (Deref r) = "[" ++ red r ++ "]"
op (Cell c) = "[" ++ c ++ "]"
op (Num i) = show i

{-
xsize Nothing = ""
xsize (Just Byte) = "byte  "
xsize (Just QWord) = "qword  "
xsize (Just DWord) = "dword  "
xsize (Just Word) = "word  "
-}

sp x = "    " ++ x ++ "  "

c' CEqual = "e"
c' CNotEqual = "ne"
c' CLess = "l"
c' CLessEq = "le"
c' CGreater = "g"
c' CGreaterEq = "ge"
c' CZero = "z"
c' CNotZero = "nz"

s' (AAdd x y) = sp "add" ++ op x ++ "," ++ op y
s' (ASub x y) = sp "sub" ++ op x ++ "," ++ op y
--s' (AMul x) = sp "mul" ++ op x
s' (AMul x) = sp "mul qword" ++ op x
s' (ADec x) = sp "dec" ++ op x
s' (ANot x) = sp "not" ++ op x
s' (AAnd x y) = sp "and" ++ op x ++ "," ++ op y
s' (AOr x y) = sp "xor" ++ op x ++ "," ++ op y
s' (AXor x y) = sp "or" ++ op x ++ "," ++ op y
s' (ASetByte c r) = sp ("set"++c' c) ++ red r
s' (ACmp x y) = sp "cmp" ++ op x ++ "," ++ op y
s' (ATest x y) = sp "test" ++ op x ++ "," ++ op y
s' (ALabel s) = s ++ ":"
s' (AJmp Nothing l) = sp "jmp" ++ l
s' (AJmp (Just c) l) = sp ("j"++c' c) ++ l
s' (ACompJmp r) = sp "jmp" ++ red r
s' (AMov x@(Cell _) y@(Num _)) = sp "mov qword" ++ op x ++ "," ++ op y
s' (AMov x y) = sp "mov" ++ op x ++ "," ++ op y



s = unlines . map s'
