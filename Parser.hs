module Parser where

import Term
import Text.ParserCombinators.Parsec

-- lex

scanInt :: Parser Int
scanInt = do
  spaces
  s <- many1 digit
  spaces
  return $ read s

keyword :: String -> Parser ()
keyword s = do
  spaces
  _ <- string s
  spaces
  return ()

-- parser

parseTRS :: Parser TRS
parseTRS = many parseRule

parseRule :: Parser Rule
parseRule = do
  l <- parseTerm
  keyword "="
  r <- parseTerm
  keyword "."

parseTerm :: Parser Term
parseTerm =
  try parseInt <|>
  try parseAdd <|>
  try parseMul

-- constants

parseInt :: Parser Term
parseInt = do
  m <- scanInt
  return $ makePaenoNum n

-- expressions

parseAdd :: Parser Term
parseAdd = do
  a <- scanInt
  keyword "+"
  b <- parseTerm
  return $ add a b
  where add 0 t = t
        add n t = App (Const "s") (add (n - 1) t)

parseMul :: Parser Term
parseMul = do
  a <- scanInt
  keyword "*"
  b <- parseTerm
  return $ mul a b
  where mul 0 t = Const "0"
        mul n t = App (App (Const "cons") t) (mul (n - 1) t)

-- parentheses

parseParen :: Parser Term
parseParen = do
  keyword "("
  t <- parseTerm
  keyword ")"
  return t

parseBrack :: Parser Term
parseBrack = do
  keyword "["
  t <- parseTerm
  keyword "]"
  return t
