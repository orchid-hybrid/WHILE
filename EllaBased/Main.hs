module Main where

import Parser
import While

process file = do
 r <- parseFile file
 case r of
  Left x -> print x
  Right (w,[]) -> mapM_ print (compile w)
