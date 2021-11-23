module Parse_expc where

import Text.ParserCombinators.Parsec

import Parse_shared
import Types


data Statement
    = CombinatoryStat String
    | ComponentStat Component
    | HaskellDefStat HaskellDef
    deriving Show

program :: Parser Program
program = f <$> many (statement <* ows)
    where
        f :: [Statement] -> Program
        f stats = foldl sorter (Program [] [] []) stats

        -- later changed to record syntax, but that's just sugar for this stuff so this keeps working...
        sorter (Program defs combs comps) stat = case stat of
            CombinatoryStat comb -> Program defs ((Combinatory comb):combs) comps
            ComponentStat comp -> Program defs combs (comp:comps)
            HaskellDefStat def -> Program (def:defs) combs comps

statement :: Parser Statement
statement
    =   try (CombinatoryStat <$> combinatory)
    <|> (ComponentStat <$> component)
    <|> (HaskellDefStat <$> (haskell_type_def <|> haskell_data_def))

combinatory :: Parser String
combinatory = string "combinatory" *> ows *> char '{' *> haskell <* char '}'

constExpr :: Parser ConstExpr
constExpr = (Constant <$> integer)
    <|> (HaskellData <$> identifier)

isoStatement :: Parser ISOStat
isoStatement
    =   try (SInput <$> (string "input" *> ws *> identifier <* ows <* char ':' <* ows) <*> (haskell_type <* char '\n'))
    <|> try (SState <$> (string "state" *> ws *> identifier <* ows <* char '=' <* ows) <*> 
        (constExpr) <*> (ows *> char ':' *> ows *> haskell_type <* char '\n'))
    <|> try (SOutput <$> (string "output" *> ws *> identifier <* ows <* char ':' <* ows) <*> (haskell_type <* char '\n'))

component :: Parser Component
component = Component
    <$> (string "component" *> ws *> identifier <* string "()" <* ows <* char '{' <* ws)
    <*> pure [] -- argument parsing part
    <*> (many (isoStatement <* ows))
    <*> (haskell_stat <* char '}')
