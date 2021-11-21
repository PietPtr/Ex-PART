{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

class Pretty a where
    pretty :: a -> String

-- .expc result

data Combinatory = Combinatory String
    deriving (Show, Eq)

type HaskellDef = String

data Component = Component {
        cmp_name :: String,
        cmp_args :: [Argument],
        cmp_isoStats :: [ISOStat],
        cmp_where :: WhereBlock
    } deriving (Show, Eq)

type Argument = String
type WhereBlock = String

type Type = String
type Name = String

data ISOStat
    = SInput Name Type
    | SState Name ConstExpr Type
    | SOutput Name Type
    deriving (Show, Eq)

inputs :: [ISOStat] -> [ISOStat]
inputs [] = []
inputs (stat:stats) = case stat of
    (SInput _ _) -> stat : inputs stats
    _ -> inputs stats

states :: [ISOStat] -> [ISOStat]
states [] = []
states (stat:stats) = case stat of
    (SState _ _ _) -> stat : states stats
    _ -> states stats

outputs :: [ISOStat] -> [ISOStat]
outputs [] = []
outputs (stat:stats) = case stat of
    (SOutput _ _) -> stat : outputs stats
    _ -> outputs stats

inputs' :: [IOStat] -> [IOStat]
inputs' [] = []
inputs' (stat:stats) = case stat of
    (Input _ _) -> stat : inputs' stats
    _ -> inputs' stats

outputs' :: [IOStat] -> [IOStat]
outputs' [] = []
outputs' (stat:stats) = case stat of
    (Output _ _) -> stat : outputs' stats
    _ -> outputs' stats

io2iso :: IOStat -> ISOStat
io2iso stat = case stat of
    (Input name t) -> (SInput name t)
    (Output name t) -> (SOutput name t)

data Program = Program {
    prg_defs :: [HaskellDef],
    prg_cmbs :: [Combinatory],
    prg_cmps :: [Component]
    } deriving (Show, Eq)

-- .expi result

type Size = (Integer, Integer)
type Coords = (CoordExpr, CoordExpr)

instance Pretty Coords where
    pretty (ce, ce') = "(" ++ pretty ce ++ ", " ++ pretty ce' ++ ")"

data CoordExpr
    = CAdd CoordExpr CoordExpr
    | CConst Integer
    | CWidth String -- component identifier
    | CHeight String
    | CX String
    | CY String
    deriving (Show, Eq)

instance Pretty CoordExpr where
    pretty (CAdd ce ce') = pretty ce ++ " + " ++ pretty ce'
    pretty (CConst c) = show c
    pretty (CWidth id) = id ++ ".w"
    pretty (CHeight id) = id ++ ".h"
    pretty (CX id) = id ++ ".x"
    pretty (CY id) = id ++ ".y"

-- TODO: grote system/program refactor: 
-- System kan na processen zijn (wat we nu 'unroll' noemen wordt dan iets uitgebreider),
-- en Program wat er na parsen uitkomt. Dan werkt alles met alleen de System en haalt daar
-- de componenten etc uit
-- Dan moet er nog een recursive ding zijn, SystemTree of zo, die de subsystems opslaat
-- Hmm of hiervan moet een postprocessed versie zijn juist... 

data System = System {
    sys_flattened :: Bool, 
    sys_id :: String, 
    sys_size :: Size,
    sys_coords :: Coords,
    sys_iodefs :: [IOStat],
    sys_instances :: [Instance],
    sys_connections :: [Connection],
    sys_repetitions :: [Repetition],
    sys_multicons :: [MultiConnection],
    sys_subsystems :: [System],
    sys_constcons :: [ConstantDriver]
    } deriving (Show, Eq)

emptySystem :: System
emptySystem = System {
        sys_flattened = False,
        sys_id = "",
        sys_size = (0, 0),
        sys_coords = (CConst 0, CConst 0),
        sys_iodefs = [],
        sys_instances = [],
        sys_connections = [],
        sys_repetitions = [],
        sys_multicons = [],
        sys_subsystems = [],
        sys_constcons = []
    }

data IOStat
    = Input Name Type
    | Output Name Type
    deriving (Show, Eq)

data Instance = Instance {
        ins_name :: String,
        ins_cmp :: Component,
        ins_args :: [ConstExpr],
        ins_size :: Size,
        ins_coords :: Coords
    } deriving (Show, Eq)

data UnplacedInstance = UnplacedInstance String [ConstExpr] Size
    deriving (Show, Eq)

data ConstExpr 
    = Constant Integer 
    | HaskellData String 
    deriving (Show, Eq)

data Connection = Connection CID CID
    deriving (Show, Eq)
data CID 
    = CID String String -- system port
    -- | ConstantDriver String
    deriving (Show, Eq, Ord)

data ConstantDriver = ConstantDriver String CID
    deriving (Show, Eq)

data MultiConnection = MultiConn MCID MCID
    deriving (Show, Eq)
data MCID = MCID String String Range -- system port range
    deriving (Show, Eq)
data Range 
    = Range Integer Integer -- from to
    | All
    deriving (Show, Eq)


data RawRepetition 
    = RawChain Name Coords [Option]
    | RawRepeat Name Coords [Option]
    deriving (Show, Eq)

data Repetition
    = Chain {
        chn_name :: String,
        chn_coords :: Coords,
        chn_unplacedInstance :: UnplacedInstance,
        chn_amount :: Integer,
        chn_layout :: String,
        chn_chainIn :: String,
        chn_chainOut :: String
    }
    | Repeat {
        rep_name :: String,
        rep_coords :: Coords,
        rep_unplacedInstance :: UnplacedInstance,
        rep_amount :: Integer,
        rep_layout :: String
    } deriving (Show, Eq)

data Option
    = Comp UnplacedInstance
    | Amount Integer
    | Layout String -- or layout expression maybe
    | Initial String 
    | ChainIn String
    | ChainOut String
    | Result String
    deriving (Show, Eq)


isoStatToBitwidth :: ISOStat -> Integer
isoStatToBitwidth stat = case stat of
    (SInput _ t) -> typeToBitwidth t
    (SOutput _ t) -> typeToBitwidth t
    _ -> error "Types.hs: Invalid ISOStatement for bitwidth (state not implemented)"

ioStatToBitWidth :: IOStat -> Integer
ioStatToBitWidth stat = case stat of
    (Input _ t) -> typeToBitwidth t
    (Output _ t) -> typeToBitwidth t

-- TODO: actually do this nicely instead of hardcoded widths for some types...
typeToBitwidth :: String -> Integer
typeToBitwidth t = case t of
        "Value" -> 16
        "Maybe Value" -> 17
        "State" -> 1
        "Vec 3 State" -> 3
        "Unsigned 2" -> 2
        "Unsigned 4" -> 4
        "Unsigned 6" -> 6
        "Unsigned 8" -> 8
        "Unsigned 16" -> 16
        "Unsigned 24" -> 24
        "Bool" -> 1
        "Bitwidth" -> 16
        "UInt" -> 32
        other -> error $ "Types.hs: Cannot find bitwidth of type " ++ other