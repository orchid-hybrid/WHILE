module Syntax (
 OpA(..), OpB(..), OpR(..),
 evalOpA, evalOpB, evalOpR,
 A(..), B(..), S(..),
 evalA, evalB, evalS,
 emptyEnvironment, lookupEnvironment, extendEnvironment
 ) where

data OpA -- Arithmetic Operator
 = Plus
 | Minus
 | Times
 deriving (Eq, Show)

data OpB -- Boolean Operator
 = And
 | Or
 deriving (Eq, Show)

data OpR -- Relational Operator
 = Equal
 | Less
 | LessEq
 | Greater
 | GreaterEq
 deriving (Eq, Show)

evalOpA Plus = (+)
evalOpA Minus = (-)
evalOpA Times = (*)

evalOpB And = (&&)
evalOpB Or = (||)

evalOpR Equal = (==)
evalOpR Less = (<)
evalOpR LessEq = (<=)
evalOpR Greater = (>)
evalOpR GreaterEq = (>=)

data A
 = V String
 | N Integer
 | AOpA OpA A A
 deriving (Eq, Show)

evalA env (V v) = lookupEnvironment env v
evalA env (N n) = n
evalA env (AOpA o p q) = evalOpA o (evalA env p) (evalA env q)

data B
 = BTrue
 | BFalse
 | BNot B
 | BOpB OpB B B
 | BOpR OpR A A
 deriving (Eq, Show)

evalB env BTrue = True
evalB env BFalse = False
evalB env (BNot m) = not (evalB env m)
evalB env (BOpB o m n) = evalOpB o (evalB env m) (evalB env n)
evalB env (BOpR o m n) = evalOpR o (evalA env m) (evalA env n)

data S
 = String := A
 | SKIP
 | S :>> S
 | IF B S S
 | WHILE B S
 deriving (Eq, Show)

evalS env (v := a) = extendEnvironment env (v, evalA env a)
evalS env SKIP = env
evalS env (p :>> q) = evalS (evalS env p) q
evalS env (IF b t e) = if evalB env b then evalS env t else evalS env e
evalS env (WHILE b s) = if evalB env b then evalS (evalS env s) (WHILE b s) else env

emptyEnvironment = []
lookupEnvironment env var = case lookup var env of Just val -> val ; Nothing -> error ("Unbound variable " ++ var)
extendEnvironment env (var', val') = (var', val') : env
