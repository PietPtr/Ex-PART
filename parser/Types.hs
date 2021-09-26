module Types where

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

data CoordExpr
    = CAdd CoordExpr CoordExpr
    | CConst Integer
    | CWidth String -- component identifier
    | CHeight String
    | CX String
    | CY String
    deriving Show

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


data IOStat
    = Input String String
    | Output String String
    deriving Show

data Instance = Instance String String [ConstExpr] Size Coords
    deriving Show
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
