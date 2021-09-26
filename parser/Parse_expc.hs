module Parse_shared where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language

import qualified Parse_shared as Shared
import Types

{-
program ::= (statement '\n')*

statement ::= (combinatory | component | haskell_type_def | haskell_data_def)

combinatory ::= '{' haskell '}'

component ::= 'component' WS identifier constantArgs '{' (iosStatement | WS)* haskell_where '}'
constantArgs ::= '(' (arg (',' arg)*)? ')'
arg ::= identifier
iosStatement ::= 'state' WS identifier WS '=' WS constant_expr WS ':' WS haskell_type '\n'
							 | ioStatement
-}