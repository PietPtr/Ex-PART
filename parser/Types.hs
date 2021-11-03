{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

class Pretty a where
    pretty :: a -> String

-- .expc result

data Combinatory = Combinatory String
    deriving Show

type HaskellDef = String

data Component = Component {
        cmp_name :: String,
        cmp_args :: [Argument],
        cmp_isoStats :: [ISOStat],
        cmp_where :: WhereBlock
    } deriving Show

type Argument = String
type WhereBlock = String

type Type = String
type Name = String

data ISOStat
    = SInput Name Type
    | SState Name ConstExpr Type
    | SOutput Name Type
    deriving Show

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
    } deriving Show

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
    deriving Show

instance Pretty CoordExpr where
    pretty (CAdd ce ce') = pretty ce ++ " + " ++ pretty ce'
    pretty (CConst c) = show c
    pretty (CWidth id) = id ++ ".w"
    pretty (CHeight id) = id ++ ".h"
    pretty (CX id) = id ++ ".x"
    pretty (CY id) = id ++ ".y"

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
    sys_subsystems :: [System]
    } deriving Show

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
        sys_subsystems = []
    }

data IOStat
    = Input Name Type
    | Output Name Type
    deriving Show

data Instance = Instance {
        ins_name :: String,
        ins_cmp :: Component,
        ins_args :: [ConstExpr],
        ins_size :: Size,
        ins_coords :: Coords
    } deriving Show

data UnplacedInstance = UnplacedInstance String [ConstExpr] Size
    deriving Show

data ConstExpr 
    = Constant Integer 
    | HaskellData String 
    deriving Show

data Connection = Connection CID CID
    deriving (Show, Eq)
data CID = CID String String -- system port
    deriving (Show, Eq, Ord)

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
    deriving Show

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
    } deriving Show

data Option
    = Comp UnplacedInstance
    | Amount Integer
    | Layout String -- or layout expression maybe
    | Initial String -- TODO: does not support both signals and constant ints, only signals
    | ChainIn String
    | ChainOut String
    | Result String
    deriving Show


-- TODO: actually do this nicely instead of hardcoded widths for 2 types...
isoStatToBitwidth :: ISOStat -> Integer
isoStatToBitwidth stat = case stat of
    (SInput _ t) -> typeToBitwidth t
    (SOutput _ t) -> typeToBitwidth t
    _ -> error "Invalid ISOStatement for bitwidth (state not implemented)"

ioStatToBitWidth :: IOStat -> Integer
ioStatToBitWidth stat = case stat of
    (Input _ t) -> typeToBitwidth t
    (Output _ t) -> typeToBitwidth t

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
        "Bool" -> 1
        other -> error $ "Cannot find bitwidth of type " ++ other