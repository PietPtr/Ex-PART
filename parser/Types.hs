{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

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

-- TODO: could have typeclasses to make this neater
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

iso2io :: ISOStat -> Maybe IOStat
iso2io stat = case stat of
    (SInput name t) -> Just (Input name t)
    (SOutput name t) -> Just (Output name t)
    _ -> Nothing

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

-- TODO (elab): grote system/program refactor: 
-- System kan na processen zijn (wat we nu 'unroll' noemen wordt dan iets uitgebreider),
-- en Program wat er na parsen uitkomt. Dan werkt alles met alleen de System en haalt daar
-- de componenten etc uit
-- Dan moet er nog een recursive ding zijn, SystemTree of zo, die de subsystems opslaat
-- Hmm of hiervan moet een postprocessed versie zijn juist... 

-- data System = System {
--     sys_flattened :: Bool, 
--     sys_name :: String, 
--     sys_size :: Size,
--     sys_coords :: Coords,
--     sys_iodefs :: [IOStat],
--     sys_instances :: [Instance],
--     sys_connections :: [Connection],
--     sys_repetitions :: [Repetition],
--     sys_multicons :: [MultiConnection],
--     sys_subsystems :: [System],
--     -- TODO (feature): constant drivers only work for components
--     sys_constantDrivers :: [ConstantDriver]
--     } deriving (Show, Eq)

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
    | Layout String
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

-- TODO (lowprio): actually do this nicely instead of hardcoded widths for some types...
typeToBitwidth :: String -> Integer
typeToBitwidth t = case t of
        "Value" -> 16
        "Maybe Value" -> 17
        "State" -> 1
        "Vec 3 State" -> 3
        "Unsigned 1" -> 1
        "Unsigned 2" -> 2
        "Unsigned 4" -> 4
        "Unsigned 5" -> 5
        "Unsigned 6" -> 6
        "Unsigned 8" -> 8
        "Unsigned 16" -> 16
        "Unsigned 24" -> 24
        "Bool" -> 1
        "Bitwidth" -> 16
        "UInt" -> 32
        "Hash" -> 128
        other -> error $ "Types.hs: Cannot find bitwidth of type " ++ other

------- Elaboration refactor

-- The design as defined in an .expc and an .expi file.
-- Easy to parse to.
data ExpcDesign = ExpcDesign {
        expcdes_defs :: [HaskellDef],
        expcdes_cmbs :: [Combinatory],
        expcdes_cmps :: [Component]
    } deriving (Show)

data Design = Design {
        des_defs :: [HaskellDef],
        des_cmbs :: [Combinatory],
        des_cmps :: [Component],
        des_systree :: SystemTree
    } deriving (Show)

data SystemTree = SystemTree {
        systr_flattened :: Bool,
        systr_name :: String,
        systr_size :: Size,
        systr_coords :: Coords,
        systr_iodefs :: [IOStat],
        systr_instances :: [Instance],
        systr_connections :: [Connection],
        systr_repetitions :: [RawRepetition],
        systr_multicons :: [MultiConnection],
        systr_subsystems :: [SystemTree],
        systr_constantDrivers :: [ConstantDriver]
    } deriving (Show)

-- The data structure after elaboration.
-- Easier to work with.
data System = System {
        sys_name :: String,
        sys_topdata :: TopData,
        sys_size :: Size,
        sys_coords :: Coords,
        sys_iodefs :: [IOStat],
        sys_elems :: [Element],
        sys_connections :: [Connection'],
        sys_constantDrivers :: [ConstantDriver]
        -- TODO: We might like a recursive system thing anyway, but it's a little bit of double admin
    } deriving (Show)

-- fake accessor, saves memory usage i guess, but takes more time, classic trade-off.
sys_subsystems :: System -> [System]
sys_subsystems system = mapMaybe toSystem (sys_elems system)
    where
        toSystem elem = case elem_implementation elem of
            (SubsysImpl sys) -> Just sys
            _ -> Nothing

sys_instances :: System -> [Instance]
sys_instances system = mapMaybe toInstance (sys_elems system)
    where
        toInstance elem = case elem_implementation elem of
            (InstanceImpl inst) -> Just inst
            _ -> Nothing

-- Pretty expensive operation now with the new data structures haha
-- But upside that it only returns the ones used in the expi (downside for the resources flow...)
-- TODO: do we want it this way, or do we want all components in the topdata?
-- Or have a sys_unique_components, perhaps.
-- sys_components :: System -> [Component]
-- sys_components system = Set.toList (sys_components' system (Set.empty))
--     where
--         sys_components' :: System -> Set Component -> Set Component
--         sys_components' system set = undefined
--             where
--                 cmps = sys_instances


data Implementation 
    = InstanceImpl Instance 
    | SubsysImpl System
    deriving Show

data Element = Element {
        elem_name :: String,
        elem_type :: String,
        elem_size :: Size,
        elem_coords :: Coords,
        elem_iodefs :: [IOStat],
        elem_implementation :: Implementation
    } deriving (Show)


class IsElement a where
    toElement :: a -> Element

instance IsElement Instance where
    toElement inst = Element {
            elem_name = ins_name,
            elem_type = cmp_name $ ins_cmp,
            elem_size = ins_size,
            elem_coords = ins_coords,
            elem_iodefs = catMaybes $ map iso2io (cmp_isoStats ins_cmp),
            elem_implementation = InstanceImpl inst
        }
        where
            Instance {..} = inst

instance IsElement System where
    toElement system = Element {
            elem_name = sys_name,
            elem_type = sys_name,
            elem_size = sys_size,
            elem_coords = sys_coords,
            elem_iodefs = sys_iodefs,
            elem_implementation = SubsysImpl system
        }
        where
            System {..} = system

data TopData
    = TopData {
        top_defs :: [HaskellDef],
        top_cmbs :: [Combinatory],
        top_cmps :: [Component]}
    | NotTop
    deriving Show


data Connection'
    = Connection' CID CID Integer -- Includes bitwidth
    deriving Show