module Parse_shared where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as P

import Types

{-
Y haskell ::= # haskell code somehow
Y haskell_where ::= # exact wat er in een haskell where block kan
Y haskell_type_def ::= # haskell type def statement
Y haskell_data_def ::= # haskell data def statement
Y haskell_type ::= WS # a haskell type
Y haskell_data ::= # some instance of a haskell data type

X constant_expr ::= haskel_data | constant_numeric_expr
X constant_numeric_expr ::= [0-9]+ | constant_numeric_expr OWS ('+' | '-' | '*') OWS constant_numeric_expr

Y WS ::= [ \n\t]+  # mandatory whitespace
Y OWS ::= [ \n\t]* # optional whitespace

P identifier ::= [a-zA-Z][a-zA-Z0-9_]*
P number ::= [0-9]+

Y ioStatement ::= 
    'input' WS identifier WS ':' WS haskell_type WS '\n'
  | 'output' WS identifier WS ':' WS haskell_type WS '\n'
-}

lexer       = P.makeTokenParser haskellDef

parens      = P.parens lexer
braces      = P.braces lexer
reserved    = P.reserved lexer
integer     = P.integer lexer
float       = P.float lexer
symbol      = P.symbol lexer

constant_expr = undefined
constant_numeric_expr = undefined

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

-- TODO: there must be a better way to do the haskell parsing, either actually follow the grammar or find a library...

-- best effort for now to get at least some string resembling a haskell type out of it...
-- Clash will error for now if the type is bullshit.
-- let's specifically disallow list types for now, as clash won't be able to work with them anyway
-- sadly also disallows record syntax
haskell_type :: Parser String
haskell_type = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '(', ')', ',', '|']

haskell_data :: Parser String
haskell_data = haskell_type

haskell_type_def :: Parser String
haskell_type_def = (\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
    string "type" <*> ows <*> haskell_type <*> string "=" <*> haskell_type

haskell_data_def :: Parser String
haskell_data_def = (\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
    string "data" <*> ows <*> haskell_type <*> string "=" <*> haskell_type -- gefeliciteerd data defs moeten nu op 1 lijn

-- TODO: dit gaat enorm stuk straks op multiline definities, dus ga nou maar de haskell grammar bouwen >:(
haskell_stat :: Parser String
haskell_stat = (many1 $ (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '(', ')', ',', '\t', '\n', '-', '<', '>', '$', '+', '-', '\'', '=', '|', ':', '*', '`']))

haskell_where :: Parser String
haskell_where = concat <$> many haskell_where_statement

haskell_where_statement :: Parser String
haskell_where_statement =  (\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
    identifier <*> ows <*> string "=" <*> ows <*> haskell_stat

-- geen records voor nu i guess...
haskell :: Parser String
haskell = many $ noneOf ['{', '}']