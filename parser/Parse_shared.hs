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
-- TODO: combinatory does not preserve leading whitespace, resulting in parse errors for e.g. datadefs and where statements

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

-- there must be a better way to do the haskell parsing, either actually follow the grammar or find a library...
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

-- TODO (lowprio): Dit is tot nu toe nog niet stuk gegaan, maar haskell op deze manier parsen is best matig
haskell_stat :: Parser String
haskell_stat = (many1 $ (oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '(', ')', ',', '\t', '\n', '-', '<', '>', '$', '+', '-', '\'', '=', '|', ':', '*', '`', '!', '?', '.', '&']))

haskell_where :: Parser String
haskell_where = concat <$> many haskell_where_statement

haskell_where_statement :: Parser String
haskell_where_statement =  (\a b c d e -> a ++ b ++ c ++ d ++ e) <$>
    identifier <*> ows <*> string "=" <*> ows <*> haskell_stat

-- geen records voor nu i guess...
haskell :: Parser String
haskell = many $ noneOf ['{', '}']