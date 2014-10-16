module Assembly (
 It(..), Condition(..), Asm(..)
 ) where

import Data.Int

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
 | ASetByte Condition (It reg)
 
 | ACmp (It reg) (It reg)
 | ATest (It reg) (It reg)
 
 | ALabel String
 | AJmp (Maybe Condition) String
 | ACompJmp reg
 | AMov (It reg) (It reg)
 deriving (Eq, Show)
