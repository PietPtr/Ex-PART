module Parse_shared where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as P

import Types



lexer       = P.makeTokenParser haskellDef

parens :: Parser a -> Parser a
parens      = P.parens lexer

braces :: Parser a -> Parser a
braces      = P.braces lexer

reserved :: String -> Parser ()
reserved    = P.reserved lexer

integer :: Parser Integer
integer     = P.integer lexer

float :: Parser Double
float       = P.float lexer

symbol :: String -> Parser String
symbol      = P.symbol lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

identifier :: Parser String
identifier = (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_' <|> char '\'')

ws :: Parser String
ws = many1 (char ' ' <|> char '\t' <|> char '\n')

ows :: Parser String
ows = many (char ' ' <|> char '\t' <|> char '\n')

ioStatement :: Parser IOStat
ioStatement
    =   Input  <$> (string "input"  *> ws *> identifier <* ows <* char ':' <* ows) <*> (haskell_type <* char '\n')
    <|> Output <$> (string "output" *> ws *> identifier <* ows <* char ':' <* ows) <*> (haskell_type <* char '\n')


-- ISSUE #1:
-- there must be a better way to do the haskell parsing, either actually follow the grammar or find a library...
-- best effort for now to get at least some string resembling a haskell type out of it...
-- Clash will error if the type is bullshit.
-- Specifically disallow list types for now, as clash won't be able to work with them anyway
-- sadly also disallows record syntax
haskell_type :: Parser String
haskell_type = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '(', ')', ',', '|']

-- ISSUE #1: Until now parsing haskell code like this hasn't failed spectacularly, but doing it this way is terrible.
haskell_stat :: Parser String
haskell_stat = (many1 $ (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '(', ')', ',', '\t', '\n', '-', '<', '>', '$', '+', '-', '\'', '=', '|', ':', '*', '`', '!', '?', '.', '&', '/', '"']))

haskell_where :: Parser String
haskell_where = concat <$> many haskell_where_statement

haskell_where_statement :: Parser String
haskell_where_statement =  (\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
    identifier <*> ows <*> string "=" <*> ows <*> haskell_stat

-- ISSUE #15: This implies we cannot use records...
haskell :: Parser String
haskell = many $ noneOf ['{', '}']