module Parse_expi where

import Prelude hiding (repeat)

import Text.ParserCombinators.Parsec

import Parse_shared 
import Types


data Statement
    = InstanceStat Instance
    | ConnectionStat Connection
    | SystemStat SystemTree
    | IOStatement IOStat
    | RepetitionStat RawRepetition
    | MultiConnStat MultiConnection
    | ConstDriverStat ConstantDriver
    deriving Show


system :: [Component] -> Parser SystemTree
system components = toSystem
    <$> isFlatenned
    <*> (identifier <* ws <* string "in" <* ws)
    <*> (size <* ws <* string "at" <* ws)
    <*> (coords <* ows) 
    <*> (f <$> (char '{' *> (system_body components) <* char '}'))
    where
        isFlatenned = option False (const True <$> (string "flatenned" <* ws))

        f :: [Statement] -> ([IOStat], [Instance], [Connection], [SystemTree], [RawRepetition], [MultiConnection], [ConstantDriver])
        f stats = foldl sorter ([], [], [], [], [], [], []) stats

        sorter (iostats, instances, connections, systems, reps, mconns, cconns) statement = case statement of
            (InstanceStat inst) -> (iostats, inst:instances, connections, systems, reps, mconns, cconns)
            (ConnectionStat conn) -> (iostats, instances, conn:connections, systems, reps, mconns, cconns)
            (SystemStat sys) -> (iostats, instances, connections, sys:systems, reps, mconns, cconns)
            (IOStatement ios) -> (ios:iostats, instances, connections, systems, reps, mconns, cconns)
            (RepetitionStat rep) -> (iostats, instances, connections, systems, rep:reps, mconns, cconns)
            (MultiConnStat mconn) -> (iostats, instances, connections, systems, reps, mconn:mconns, cconns)
            (ConstDriverStat cconn) -> (iostats, instances, connections, systems, reps, mconns, cconn:cconns)

        toSystem flattened name size coords (iostats, instances, connections, systems, reps, mconns, cconns) =
            SystemTree {
                systr_flattened = flattened,
                systr_name = name,
                systr_size = size,
                systr_coords = coords,
                systr_iodefs = iostats,
                systr_instances = instances,
                systr_connections = connections,
                systr_repetitions = reps,
                systr_multicons = mconns,
                systr_subsystems = systems,
                systr_constantDrivers = cconns
            }




system_body :: [Component] -> Parser [Statement]
system_body components = many1 (anystat <* ows)
    where
        anystat = try (IOStatement <$> (ows *> ioStatement) <* whiteSpace)
            <|> try (MultiConnStat <$> (ows *> multiconnection <* char '\n' <* whiteSpace))
            <|> try (ConnectionStat <$> (ows *> connection <* char '\n' <* whiteSpace))
            <|> try (ConstDriverStat <$> (ows *> constant_driver_stat <* char '\n' <* whiteSpace))
            <|> try (RepetitionStat <$> (ows *> (repeat <|> chain)) <* whiteSpace)
            <|> try (InstanceStat <$> (ows *> (cmp_instance components) <* char '\n' <* whiteSpace))
            <|> (SystemStat <$> (ows *> (system components)) <* whiteSpace)

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

-- TODO (feature): layout expressions
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
            _ -> error $ "Parse_expi.hs: Component " ++ name ++ " not in .expc file."

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


ltr_rtl :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
ltr_rtl f p_from p_to = try ltr <|> rtl
    where
        ltr = f
            <$> (p_from <* ows <* string "->" <* ows)
            <*> p_to
        rtl = (\b a -> f a b)
            <$> (p_to <* ows <* string "<-" <* ows)
            <*> p_from

connection :: Parser Connection
connection = ltr_rtl Connection cid cid


cid :: Parser CID
cid = 
    (try (CID <$> (subbedId <* char '.') <*> identifier))
    <|> ((CID "this") <$> identifier)
    where
        subbedId = (++) <$> identifier <*> (option "" subscript)
        subscript = (\i -> '_' : show i) <$> (char '[' *> integer <* char ']')

multiconnection :: Parser MultiConnection
multiconnection = ltr_rtl MultiConn mcid mcid

mcid :: Parser MCID
mcid = 
    try (construct <$> identifier <*> (range <* char ':') <*> identifier)
    <|> (construct <$> identifier <*> (pure All <* char ':') <*> identifier)
    where
        construct s r p = MCID s p r
        range = Range <$> (char '[' *> integer) <*> (char '-' *> integer <* char ']')

constant_driver_stat :: Parser ConstantDriver
constant_driver_stat = ltr_rtl ConstantDriver constant_driver cid

constant_driver :: Parser String
constant_driver = (\pl c pr -> pl ++ c ++ pr) <$> 
    (string "(") *> ((\c -> show (read c :: Int)) <$> const) <* (string ")")
    where
        -- Only numeric constants are supported.
        symbols = ['0'..'9']
        text = (many1 $ oneOf symbols)

        const = 
            try ((\tl pl c pr tr -> tl ++ pl ++ c ++ pr ++ tr) <$> 
                (option "" text) <*> string "(" <*> const <*> string ")" <*> (option "" text))
            <|> text

