module Parse_expi where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language

import Parse_shared 
import Types

{-
program ::= system # a program is exactly one system - the top entity.

system ::= (flattened WS)? identifier WS 'in' WS size WS 'at' WS coords '{' 
				(ioStatement | WS)* (instance | connection | system | WS)*  # waar is repitition hier?
		'}'

Y component_instantiation ::= identifier WS 'is' WS identifier '(' (arg (',' arg)*)? ')' WS 'in' WS size 
Y instance ::=  component_instantiation WS 'at' WS coords '\n'
X arg ::= haskell_type OWS ':' OWS 'Type'
			| constant_expr OWS ':' OWS 'Const'
Y size ::= '(' OWS number OWS ',' OWS number OWS ')'
Y coords ::= '(' OWS coord_expr OWS ',' OWS coord_expr OWS ')'
Y coord_expr ::= (number | identifier '.' ('w','h','x','h') | coord_expr '+' coord_expr)

Y connection ::= identifier ('.' identifier)? ('<-' | '->') identifier ('.' identifier)? '\n'

X repetition ::= identifier WS ('repeat' | 'chain') WS 'at' coords '{' (identifier OWS '=' OWS repeat_data '\n')* '}'
X repeat_data ::= coords | number | identifier | component_instantiation
-}

data Statement
    = InstanceStat Instance
    | ConnectionStat Connection
    | SystemStat System
    | IOStatement IOStat
    deriving Show


system :: [Component] -> Parser System
system components = toSystem
    <$> isFlatenned
    <*> (identifier <* ws <* string "in" <* ws)
    <*> (size <* ws <* string "at" <* ws)
    <*> (coords <* ows) 
    <*> (f <$> (char '{' *> (system_body components) <* char '}'))
    where
        isFlatenned = option False (const True <$> (string "flatenned" <* ws))

        f :: [Statement] -> ([IOStat], [Instance], [Connection], [System])
        f stats = foldl sorter ([], [], [], []) stats

        sorter (iostats, instances, connections, systems) statement = case statement of
            (InstanceStat inst) -> (iostats, inst:instances, connections, systems)
            (ConnectionStat conn) -> (iostats, instances, conn:connections, systems)
            (SystemStat sys) -> (iostats, instances, connections, sys:systems)
            (IOStatement ios) -> (ios:iostats, instances, connections, systems)

        toSystem flattened name size coords (iostats, instances, connections, systems) =
            System {
                sys_flattened = flattened,
                sys_id = name,
                sys_size = size,
                sys_coords = coords,
                sys_iodefs = iostats,
                sys_instances = instances,
                sys_connections = connections,
                sys_repetitions = [],
                sys_subsystems = systems}

system_body :: [Component] -> Parser [Statement]
system_body components = many1 (anystat <* ows)
    where
        anystat = try (IOStatement <$> (ows *> ioStatement))
            <|> try (InstanceStat <$> (ows *> (cmp_instance components) <* char '\n'))
            <|> try (ConnectionStat <$> (ows *> connection <* char '\n'))
            <|> (SystemStat <$> (ows *> (system components)))

cmp_instance :: [Component] -> Parser Instance
cmp_instance components = Instance
    <$> (identifier <* ws <* string "is" <* ws) -- identifier
    <*> (findCmp <$> (identifier <* ws <* string "in" <* ws)) -- generic component name
    <*> (pure []) -- arguments
    <*> (size <* ws <* string "at" <* ws) -- size
    <*> coords -- coords
    where
        findCmp :: String -> Component
        findCmp name = case filter (\c -> name == cmp_name c) components of
            (x:_) -> x
            _ -> error $ "Component " ++ name ++ " not in .expc file."

size :: Parser Size
size = (,) <$> (char '(' *> integer <* char ',') <*> (integer <* char ')')

coords :: Parser Coords
coords = (,) 
    <$> (char '(' *> ows *> coord_expr <* ows <* char ',') 
    <*> (ows *> coord_expr <* ows <* char ')')

coord_expr :: Parser CoordExpr
coord_expr =
    try (CAdd <$> (coord_bottom <|> coord_expr) <*> (ows *> char '+' *> ows *> (coord_bottom <|> coord_expr)))
    <|> coord_bottom

coord_bottom :: Parser CoordExpr
coord_bottom
    =   (CConst  <$> integer)
    <|> try (CWidth  <$> identifier <* char '.' <* char 'w' )
    <|> try (CHeight <$> identifier <* char '.' <* char 'h' )
    <|> try (CX      <$> identifier <* char '.' <* char 'x' )
    <|>     (CY      <$> identifier <* char '.' <* char 'y' )


connection :: Parser Connection
connection = try ltr <|> rtl
    where
        ltr = Connection
            <$> (cid <* ows <* string "->" <* ows)
            <*> (cid)
        rtl = (\to from -> Connection from to)
            <$> (cid <* ows <* string "<-" <* ows)
            <*> (cid)

cid :: Parser CID
cid = try (CID <$> (identifier <* char '.') <*> identifier)
    <|> ((CID "this") <$> identifier)