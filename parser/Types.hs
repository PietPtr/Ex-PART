{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Maybe
import Debug.Trace

class Pretty a where
    pretty :: a -> String

type Size = (LayoutExpr, LayoutExpr)
type Coords = (LayoutExpr, LayoutExpr)

instance Pretty Coords where
    pretty (ce, ce') = "(" ++ pretty ce ++ ", " ++ pretty ce' ++ ")"

data LayoutExpr
    = CAdd LayoutExpr LayoutExpr
    | CSub LayoutExpr LayoutExpr
    | CConst Integer
    | CWidth String -- component identifier
    | CHeight String
    | CX String
    | CY String
    deriving (Show, Eq)

instance Pretty LayoutExpr where
    pretty (CAdd ce ce') = pretty ce ++ " + " ++ pretty ce'
    pretty (CSub ce ce') = pretty ce ++ " - " ++ pretty ce'
    pretty (CConst c) = show c
    pretty (CWidth id) = id ++ ".w"
    pretty (CHeight id) = id ++ ".h"
    pretty (CX id) = id ++ ".x"
    pretty (CY id) = id ++ ".y"


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



data IOStat
    = Input Name Type
    | Output Name Type
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
        "Unsigned 3" -> 3
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
        "MyHash" -> 32
        "Maybe Hash" -> 129
        "DataCtrState" -> 3
        "MD5State" -> 7
        "InWord" -> 16
        ('M':'a':'y':'b':'e':' ':x) -> typeToBitwidth x + 1
        other -> error $ "Types.hs: Cannot find bitwidth of type " ++ other



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
        systr_unplaced :: Bool,
        systr_name :: String,
        systr_type :: String,
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


data Combinatory = Combinatory String
    deriving (Show, Eq)

type HaskellDef = String

data Component = Component {
        cmp_name :: String,
        cmp_args :: [Argument],
        cmp_isoStats :: [ISOStat],
        cmp_where :: WhereBlock
    } deriving (Show, Eq)

data Instance 
    = CmpInstance {
        cins_name :: String,
        cins_cmp :: Component,
        cins_args :: [ConstExpr],
        cins_size :: Size,
        cins_coords :: Coords} 
    | SysInstance {
        sins_name :: String,
        sins_sysname :: String,
        sins_size :: Size,
        sins_coords :: Coords
    } deriving (Show, Eq)

ins_isCmpInstance :: Instance -> Bool
ins_isCmpInstance CmpInstance{} = True
ins_isCmpInstance _ = False

ins_name :: Instance -> String
ins_name CmpInstance{cins_name=n} = n
ins_name SysInstance{sins_name=n} = n

ins_size :: Instance -> Size
ins_size CmpInstance{cins_size=n} = n
ins_size SysInstance{sins_size=n} = n

ins_coords :: Instance -> Coords
ins_coords CmpInstance{cins_coords=n} = n
ins_coords SysInstance{sins_coords=n} = n

type Argument = String
type WhereBlock = String

type Type = String
type Name = String


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

-- The data structure after elaboration.
-- Easier to work with.
data System = System {
        sys_unplaced :: Bool,
        sys_name :: String,
        sys_type :: String,
        sys_topdata :: TopData,
        sys_size :: Size,
        sys_coords :: Coords,
        sys_iodefs :: [IOStat],
        sys_elems :: [Element],
        sys_allElems :: [Element], -- including the unplaced ones
        sys_connections :: [Connection'],
        sys_constantDrivers :: [ConstantDriver]
    } deriving (Show)

-- fake accessor, saves memory usage i guess, but takes more time, classic trade-off.
sys_subsystems :: System -> [System]
sys_subsystems system = mapMaybe toSystem (sys_elems system)
    where
        toSystem elem = case elem_implementation elem of
            (SubsysImpl sys) -> Just sys 
            _ -> Nothing

sys_subsystems' :: System -> [System]
sys_subsystems' system = mapMaybe toSystem (sys_allElems system)
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

sys_istop :: System -> Bool
sys_istop system = case sys_topdata system of
    TopData{} -> True
    NotTop -> False

data Implementation 
    = InstanceImpl Instance 
    | SubsysImpl System
    deriving Show

data Element = Element {
        elem_unplaced :: Bool,
        elem_name :: String,
        elem_type :: String,
        elem_size :: Size,
        elem_coords :: Coords,
        elem_iodefs :: [IOStat],
        elem_implementation :: Implementation
    } deriving (Show)

elem_isSystem :: Element -> Bool
elem_isSystem e = case elem_implementation e of
    (SubsysImpl _) -> True
    _ -> False

class IsElement a where
    toElement :: a -> Element

instance IsElement Instance where
    toElement inst@CmpInstance{..} = Element {
            elem_unplaced = False,
            elem_name = cins_name,
            elem_type = cmp_name $ cins_cmp,
            elem_size = cins_size,
            elem_coords = cins_coords,
            elem_iodefs = catMaybes $ map iso2io (cmp_isoStats cins_cmp),
            elem_implementation = InstanceImpl inst
        }
    toElement SysInstance{..} = error 
        "Types.hs: Cannot convert SysInstance to element, convert the SysInstance to a System first, then call toElement."
    

instance IsElement System where
    toElement system = Element {
            elem_unplaced = sys_unplaced,
            elem_name = sys_name,
            elem_type = sys_type,
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

constPrefix = "$const_"

-- TODO (feature): een standaard library van mealy machines zou niet misstaan