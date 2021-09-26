module Types where

-- .expc result

data Combinatory = Combinatory String
    deriving Show

type HaskellDef = String
data Component = Component String [Argument] [IOSStat] WhereBlock
    deriving Show
type Argument = String
type WhereBlock = String
data IOSStat
    = SInput String String
    | State String ConstExpr String
    | SOutput String String
    deriving Show

data Program = Program [HaskellDef] [Combinatory] [Component]
    deriving Show

type Size = (Integer, Integer)
type Coords = (Integer, Integer) -- tijdens parsen al reducen?

-- .expi result

data System = System {
    flattened :: Bool,
    sys_id :: String,
    size :: Size,
    coords :: Coords,
    iodefs :: [IOStat],
    instances :: [Instance],
    connections :: [Connection],
    repetitions :: [Repetition],
    subsystems :: [System]
    } deriving Show


data IOStat
    = Input String String
    | Output String String
    deriving Show

data Instance = Instance String String [ConstExpr] Size Coords
    deriving Show
data UnplacedInstance = UnplacedInstance String String [ConstExpr] Size
    deriving Show
data ConstExpr = HaskellData String | Constant Integer -- hier ook al lekker reducen dan?
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
