module Parse_expc where

import Text.ParserCombinators.Parsec

import Parse_shared
import Types


data Statement
    = HaskellStat String
    | ComponentStat Component
    deriving Show

-- There seem to be some issues with parsing if the expc file starts with an empty line
-- ISSUE 31: I don't think comments outside haskell expressions actually work in expc files
expcdesign :: Parser ExpcDesign
expcdesign = f <$> many (statement <* ows)
    where
        f :: [Statement] -> ExpcDesign
        f stats = foldl sorter (ExpcDesign [] []) stats

        -- later changed to record syntax, but that's just sugar for this stuff so this keeps working...
        sorter (ExpcDesign combs comps) stat = case stat of
            HaskellStat comb -> ExpcDesign ((HaskellDef comb):combs) comps
            ComponentStat comp -> ExpcDesign combs (comp:comps)

statement :: Parser Statement
statement
    =   try (HaskellStat <$> haskell_block)
    <|> (ComponentStat <$> component)

haskell_block :: Parser String
haskell_block = string "haskell" *> ows *> char '{' *> haskell <* char '}'

constExpr :: Parser ConstExpr
constExpr = (Constant <$> integer)
    <|> (HaskellData <$> identifier)

isoStatement :: Parser ISOStat
isoStatement
    =   try (SInput <$> (string "input" *> ws *> identifier <* ows <* char ':' <* ows) <*> (haskell_type <* char '\n'))
    <|> try (SState <$> (string "state" *> ws *> identifier <* ows <* char '=' <* ows) <*> 
        (constExpr) <*> (ows *> char ':' *> ows *> haskell_type <* char '\n')) -- ISSUE 10: somehow add a nice error when the initial state is missing
    <|> try (SOutput <$> (string "output" *> ws *> identifier <* ows <* char ':' <* ows) <*> (haskell_type <* char '\n'))

component :: Parser Component
component = Component
    <$> (string "component" *> ws *> identifier <* string "()" <* ows <* char '{' <* ws)
    <*> pure [] -- argument parsing part
    <*> (many (isoStatement <* ows))
    <*> (haskell_stat <* char '}')
