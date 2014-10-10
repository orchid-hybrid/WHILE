module Parser (
 parseFile
 ) where

import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative
import System.IO

import Syntax

brackets p = between (char '(' <* skipSpaces) (char ')') p

parseOpA = choice [ do string "+" ; return Plus
                  , do string "-" ; return Minus
                  , do string "*" ; return Times
                  ]

parseOpB = choice [ do string "&" ; return And
                  , do string "|" ; return Or
                  ]

parseOpR = choice [ do string "=" ; return Equal
                  , do string "<" ; return Less
                  , do string "<=" ; return LessEq
                  , do string ">" ; return Greater
                  , do string ">=" ; return GreaterEq
                  ]

parseV = do v <- munch1 isAlpha ; return (V v)
parseN = do n <- munch1 isDigit ; return (N (read n))
parseA = do chainl1 (terminalA <* skipSpaces) (parseOpA' <* skipSpaces)
 where parseOpA' = do o <- parseOpA ; return (AOpA o)
terminalA = choice [ parseV
                   , parseN
                   , brackets parseA
                   ]

parseB = do chainl1 (terminalB <* skipSpaces) (parseOpB' <* skipSpaces)
 where parseOpB' = do o <- parseOpB ; return (BOpB o)

terminalB = choice [ do string "true" ; return (BTrue)
                   , do string "false" ; return (BFalse)
                   , do string "not " ; skipSpaces
                        m <- terminalB
                        return (BNot m)
                   , do m <- parseA
                        o <- parseOpR ; skipSpaces
                        n <- parseA
                        return (BOpR o m n)
                   , brackets parseB
                   ]

parseS = do chainl1 (s <* skipSpaces) spacer
 where spacer = do char ';' +++ char '\n' ; skipSpaces ; return (:>>)
       s = choice [ do v <- munch1 isAlpha ; skipSpaces
                       string ":=" ; skipSpaces
                       a <- parseA
                       return (v := a)
                  , do string "skip"
                       return SKIP
                  , do string "if" ; skipSpaces
                       b <- parseB
                       string "then" ; skipSpaces
                       t <- parseS
                       string "else" ; skipSpaces
                       e <- parseS
                       string "end"
                       return (IF b t e)
                  , do string "while" ; skipSpaces
                       b <- parseB
                       string "do" ; skipSpaces
                       s <- parseS
                       string "end" ; skipSpaces
                       return (WHILE b s)
                  ]

parseFile filename = do s <- readFile filename
                        let p = filter ((=="") . snd) . readP_to_S (skipSpaces >> parseS) $ s
                        return $ case p of
                         [] -> Left "Could not parse"
                         [r] -> Right r
                         _ -> Left "Ambiguous parses"


