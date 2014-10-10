module X86 where

import Data.Int (Int64(..))

-- this represents the level of indirection of a value
data I a = Concrete a | Pointer a deriving (Eq, Show)

data Register = RAX | RBX | RCX | RDX
              | RSI | RDI | RSP | RBP
              | R8  | R9  | R10 | R11
              | R12 | R13 | R14 | R15
              deriving (Eq, Show)

data Operand = Register  (I Register)
             | Memory    (I Int64)
             | Stack     (I Int64)
             | Immediate (I Int64)
             | Label_R   String
             deriving (Eq, Show)

data Instruction = Label String
                 | ADD  Operand Operand
                 | CMP  Operand Operand
                 | DIV  Operand
                 | JE   Operand
                 | JMP  Operand
                 | JNE  Operand
                 | MOV  Operand Operand
                 | MUL  Operand Operand
                 | NOP
                 | PUSH Operand
                 | POP  Operand
                 | SUB  Operand Operand
                 deriving (Eq, Show)
