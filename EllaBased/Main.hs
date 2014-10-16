module Main where

import Parser
import While
import Assembly

process file = do
 r <- parseFile file
 case r of
  Left x -> print x
  Right (w,[]) -> putStrLn (s . compile $ w)
