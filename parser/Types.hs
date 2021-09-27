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

data ISOStat
    = SInput String String
    | SState String ConstExpr String
    | SOutput String String
    deriving Show

data Program = Program [HaskellDef] [Combinatory] [Component]
    deriving Show



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
        sys_subsystems = []
    }

data IOStat
    = Input String String
    | Output String String
    deriving Show

data Instance = Instance {
        ins_name :: String,
        ins_cmp :: String,
        ins_args :: [ConstExpr],
        ins_size :: Size,
        ins_coords :: Coords
    } deriving Show

data UnplacedInstance = UnplacedInstance String String [ConstExpr] Size
    deriving Show
data ConstExpr 
    = Constant Integer -- hier ook al lekker reducen dan?
    -- | HaskellData String -- komt in Version Two
    deriving Show

data Connection = Connection CID CID
    deriving Show
data CID = CID String String
    deriving Show

data Repetition 
    = Chain [Option]
    | Fold [Option]
    deriving Show

data Option
    = Comp UnplacedInstance
    | Amount Integer
    | Layout String -- or layout expression maybe
    | Initial (Either String Integer) -- supports both signals and constants
    | ChainIn String
    | ChainOut String
    | OutputName String
    deriving Show
