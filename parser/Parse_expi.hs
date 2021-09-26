module Parse_expi where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language

import qualified Parse_shared as Shared
import Types

{-
program ::= system # a program is exactly one system - the top entity.

system ::= identifier WS 'in' WS size WS 'at' WS coords '{' 
				(ioStatement | WS)* (instance | connection | system | WS)*  # waar is repitition hier?
		'}'

component_instantiation ::= identifier WS 'is' WS identifier '(' (arg (',' arg)*)? ')' WS 'in' WS size 
instance ::= ('flattened' WS)? component_instantiation WS 'at' WS coords '\n'
arg ::= haskell_type OWS ':' OWS 'Type'
			| constant_expr OWS ':' OWS 'Const'
size ::= '(' OWS number OWS ',' OWS number OWS ')'
coords ::= '(' OWS coord_expr OWS ',' OWS coord_expr OWS ')'
coord_expr ::= (number | identifier '.' ('w','h','x','h') | coord_expr '+' coord_expr)

connection ::= identifier ('.' identifier)? ('<-' | '->') identifier ('.' identifier)? '\n'

repetition ::= identifier WS ('repeat' | 'chain') WS 'at' coords '{' (identifier OWS '=' OWS repeat_data '\n')* '}'
repeat_data ::= coords | number | identifier | component_instantiation
-}