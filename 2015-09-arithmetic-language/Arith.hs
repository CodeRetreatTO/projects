module Arithmetic where

-- Kader, inaimathi, Dann

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec

-- "1 + 2" -- 3
-- "2 - 1" -- 1
-- "4 * 2" -- 8
-- "8 / 2" -- 4
-- "7 / 2" -- ??
-- "6 / 0" -- ??
-- "a * 2" -- ??
-- "4 - 5 + 6 * 2" -- ??

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Div Exp Exp
         | Mul Exp Exp
         | Num Int deriving (Eq, Show)

process str = case parse parseExp "blargh" str of
                Right exp -> eval exp
                _ -> undefined

parseExp :: Parser Exp
parseExp = try recur <|> parseNum
    where recur = do n <- parseNum
                     spaces
                     op <- parseOp
                     spaces
                     rest <- parseExp
                     return $ op n rest

parseOp :: Parser (Exp -> Exp -> Exp)
parseOp = do elem <- oneOf "+-/*"
             return $ case elem of
                        '+' -> Add
                        '-' -> Sub
                        '/' -> Div
                        '*' -> Mul
                        _ -> undefined

parseNum :: Parser Exp
parseNum = liftM (Num . read) $ many1 digit

eval :: Exp -> Int
eval (Num n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Mul a b) = (eval a) * (eval b)
eval (Div a b) = (eval a) `div` (eval b)
