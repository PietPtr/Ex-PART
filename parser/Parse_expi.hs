module Parse_expi where

import Prelude hiding (repeat)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language

import Parse_shared 
import Types
-- TODO: Waarom is er hier zo veel in snake_case?

{-
program ::= system # a program is exactly one system - the top entity.

system ::= (flattened WS)? identifier WS 'in' WS size WS 'at' WS coords '{' 
				(ioStatement | WS)* (instance | connection | system | WS)*  # waar is repetition hier?
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
    | RepetitionStat RawRepetition
    | MultiConnStat MultiConnection
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

        f :: [Statement] -> ([IOStat], [Instance], [Connection], [System], [RawRepetition], [MultiConnection])
        f stats = foldl sorter ([], [], [], [], [], []) stats

        sorter (iostats, instances, connections, systems, reps, mconns) statement = case statement of
            (InstanceStat inst) -> (iostats, inst:instances, connections, systems, reps, mconns)
            (ConnectionStat conn) -> (iostats, instances, conn:connections, systems, reps, mconns)
            (SystemStat sys) -> (iostats, instances, connections, sys:systems, reps, mconns)
            (IOStatement ios) -> (ios:iostats, instances, connections, systems, reps, mconns)
            (RepetitionStat rep) -> (iostats, instances, connections, systems, rep:reps, mconns)
            (MultiConnStat mconn) -> (iostats, instances, connections, systems, reps, mconn:mconns)

        toSystem flattened name size coords (iostats, instances, connections, systems, reps, mconns) =
            System {
                sys_flattened = flattened,
                sys_id = name,
                sys_size = size,
                sys_coords = coords,
                sys_iodefs = iostats,
                sys_instances = instances,
                sys_connections = connections,
                sys_repetitions = map fitRepetition reps,
                sys_multicons = mconns,
                sys_subsystems = systems
            }


fitRepetition :: RawRepetition -> Repetition
fitRepetition rep = repetition
    where
        repetition = case rep of
            (RawRepeat name coords options) -> Repeat {
                    rep_name = name,
                    rep_coords = coords,
                    rep_unplacedInstance = unplacedInstance options,
                    rep_amount = amount options,
                    rep_layout = layout options
                }
            (RawChain name coords options) -> Chain {
                    chn_name = name,
                    chn_coords = coords,
                    chn_unplacedInstance = unplacedInstance options,
                    chn_amount = amount options,
                    chn_layout = layout options,
                    chn_chainIn = case [ x | ChainIn x <- options ] of
                        [] -> error "Missing option `chain_in` in chain statement"
                        (x:_) -> x,
                    chn_chainOut = case [ x | ChainOut x <- options ] of
                        [] -> error "Missing option `chain_out` in chain statement"
                        (x:_) -> x
            }

        unplacedInstance options = case [ x | Comp x <- options ] of
            [] -> error "Missing option `component` in repetition statement."
            (x:_) -> x
        amount options = case [ x | Amount x <- options ] of
            [] -> error "Missing option `amount` in a repetition statement."
            (x:_) -> x
        layout options = case [x | Layout x <- options ] of
            [] -> error "Missing option `layout` in a repetition statement."
            (x:_) -> x


system_body :: [Component] -> Parser [Statement]
system_body components = many1 (anystat <* ows)
    where
        anystat = try (IOStatement <$> (ows *> ioStatement))
            <|> try (MultiConnStat <$> (ows *> multiconnection <* char '\n'))
            <|> try (ConnectionStat <$> (ows *> connection <* char '\n'))
            <|> try (RepetitionStat <$> (ows *> (repeat <|> chain)))
            <|> try (InstanceStat <$> (ows *> (cmp_instance components) <* char '\n'))
            <|> (SystemStat <$> (ows *> (system components)))

chain :: Parser RawRepetition
chain = RawChain
    <$> (string "chain" *> ws *> identifier <* ws <* string "at" <* ws)
    <*> (coords <* ows <* char '{' <* ows)
    <*> (option_stats <* ows <* char '}')

repeat :: Parser RawRepetition
repeat = RawRepeat
    <$> (string "repeat" *> ws *> identifier <* ws <* string "at" <* ws)
    <*> (coords <* ows <* char '{' <* ows)
    <*> (option_stats <* ows <* char '}')

option_stats :: Parser [Option]
option_stats = (ows *> option_stat <* ows) `sepBy` (char ',')

option_stat :: Parser Option
option_stat 
    =   Amount <$> option_parser "amount" integer
    <|> Layout <$> option_parser "layout" layout 
    <|> Initial <$> option_parser "initial" identifier
    <|> Result <$> option_parser "result" identifier 
    <|> (try $ Comp <$> option_parser "component" unplaced_instance)
    <|> (try $ ChainIn <$> option_parser "chain_in" identifier)
    <|> ChainOut <$> option_parser "chain_out" identifier
    where
        option_parser str p = (string str *> ows *> char '=' *> ows *> p)

-- TODO: layout expressions
layout :: Parser String
layout = string "vertical" <|> string "horizontal"

unplaced_instance :: Parser UnplacedInstance
unplaced_instance = UnplacedInstance
    <$> (identifier <* ws <* string "in" <* ws)
    <*> (pure [])
    <*> (size)

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


ltr_rtl :: (a -> a -> b) -> Parser a -> Parser b
ltr_rtl f p = try ltr <|> rtl
    where
        ltr = f
            <$> (p <* ows <* string "->" <* ows)
            <*> p
        rtl = (\b a -> f a b)
            <$> (p <* ows <* string "<-" <* ows)
            <*> p

connection :: Parser Connection
connection = ltr_rtl Connection cid

cid :: Parser CID
cid = 
    (try (CID <$> (subbedId <* char '.') <*> identifier))
    <|> ((CID "this") <$> identifier)
    where
        subbedId = (++) <$> identifier <*> (option "" subscript)
        subscript = (\i -> '_' : show i) <$> (char '[' *> integer <* char ']')

multiconnection :: Parser MultiConnection
multiconnection = ltr_rtl MultiConn mcid

mcid :: Parser MCID
mcid = 
    try (construct <$> identifier <*> (range <* char ':') <*> identifier)
    <|> (construct <$> identifier <*> (pure All <* char ':') <*> identifier)
    where
        construct s r p = MCID s p r
        range = Range <$> (char '[' *> integer) <*> (char '-' *> integer <* char ']')
        
