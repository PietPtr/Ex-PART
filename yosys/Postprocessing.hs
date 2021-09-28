module Postprocessing where

import Data.Aeson
import Debug.Trace
import qualified Data.Map as Map

import Types

-- definieer een mooi datatype, bouw bestaande System/Program om hiernaartoe, het nice makend
-- definieer dan een toJSON hiervoor, verander het naar een Value, en prop het in de bestaande JSON
data Module = Module {
    mod_name :: String,
    mod_top :: Bool, -- all attributes...
    mod_ports :: [Port],
    mod_cells :: [Cell] 
    } deriving Show

data Port = Port {
    port_name :: String,
    port_direction :: PortDir,
    port_bits :: [Integer]
    } deriving Show

data PortDir = In | Out
    deriving Show

data Cell = Cell {
    cell_name :: String,
    cell_type :: String,
    -- cell_portDirections :: [Port] -- copy this from the type / module (Program => Component => ISOStats)
    cell_connections :: [CellConn]
    } deriving Show

--                       portid  "bits"
data CellConn = CellConn String [Integer]
    deriving Show

type Net = [Integer] -- directly a yosys "bits" entry

collatzer :: Module
collatzer = Module {
    mod_name = "collatzer",
    mod_top = False,
    mod_ports = [
        Port {
            port_name = "val_in",
            port_direction = In,
            port_bits = [ 5, 6, 7, 8, 9, 10, 11, 12 ]
        },
        Port {
            port_name = "val_out",
            port_direction = Out,
            port_bits = [ {- een range van 8 bits -} ]
        }
    ],
    mod_cells = [
        Cell {
            cell_name = "router_instance_router",
            cell_type = "router",
            cell_connections = [
                CellConn "clk" [2],
                CellConn "rst" [3],
                CellConn "en" [4],
                CellConn "val" [ {- bepaalde nets, hier die van val_in -} ],
                CellConn "odd" [ {- 'nieuwe' nets -} ],
                CellConn "even" [ {--} ]
            ]
        }
        -- en nog 3 cells voor de onOdd, onEven, en merger
    ]
}

parseTest :: IO (Maybe Value)
parseTest = decodeFileStrict "testenv/synthesized.json"

makeTopModule :: Program -> System -> [Module]
makeTopModule program system = makeModules True program system

-- TODO: beware that it may be necessary to introduce a bookkeeping datastructure, create a record for this.

makeModules :: Bool -> Program -> System -> [Module]
makeModules isTop p@(Program _ _ components) system = mod : 
    (concat $ map (makeModules False p) (sys_subsystems system))
    where
        mod = Module {
                mod_name = sys_id system,
                mod_top = isTop,
                mod_ports = makePorts 5 $ sys_iodefs system,
                mod_cells = makeCells nextBit components (sys_connections system) (sys_instances system)
            }
        nextBit = 1 + (last $ port_bits $ last $ mod_ports mod)

-- bitCtr counts which netnumber is free
makePorts :: Integer -> [IOStat] -> [Port]
makePorts bitCtr [] = []
makePorts bitCtr (stat:stats) = 
    Port {
        port_name = name,
        port_direction = direction,
        port_bits = [bitCtr..(bitCtr+bitwidth-1)]
    } : makePorts (bitCtr+bitwidth) stats
    where
        (direction, name, t) = case stat of
            (Input iname it) -> (In, iname, it)
            (Output oname ot) -> (Out, oname, ot)
        bitwidth = typeToBitwidth t


-- creates a net map for this system (only the top system)
nets :: [Component] -> System -> [Connection] -> Map.Map CID Net
nets comps system connections = nets' comps system 6 connections Map.empty

nets' :: [Component] -> System -> Integer -> [Connection] -> Map.Map CID Net -> Map.Map CID Net
nets' _ _ _ [] map = map
nets' comps system bitCtr ((Connection from to):connections) map = 
    nets' comps system (bitCtr+netBitwidth) connections map'
    where
        map' = Map.insert from net map
        net = [bitCtr..(bitCtr+netBitwidth-1)]
        (CID from_cmp_name from_port) = from

        netBitwidth = if from_cmp_name == "this"
            then ioStatToBitWidth io_statement
            else isoStatToBitwidth iso_statement
            -- TODO: subsystem connections kan hij niet vinden

        ---- then part van de netbitwidth if statement
        io_statement = case filter findIOStat (sys_iodefs system) of
            (x:_) -> x
            [] -> error "cannot find IO statement."
        findIOStat stat = case stat of
            (Output name _) -> name == from_port
            (Input name _) -> name == from_port

        ---- alle meuk voor de else part van de netbitwidth if statement
        -- in instances from_cmp_name opzoeken
        -- check `this` hier...
        component_name = ins_cmp $ case filter 
            (\i -> ins_name i == from_cmp_name) 
            (sys_instances system) of
                (x:_) -> x
                [] -> error $ "cannot find component name " ++ from_cmp_name ++ " in " ++ (show $ sys_instances system)
        -- in components vind component met die naam
        component = case filter (\c -> cmp_name c == component_name) comps of
            (x:_) -> x
            [] -> error $ "cannot find component " ++ component_name
        -- in isostats, vind port: de bitwidth van de from_port ophalen
        iso_statement = case filter findStat (cmp_isoStats component) of
            (x:_) -> x
            [] -> error $ "cannot find iso statement " ++ from_port
        findStat stat = case stat of
            (SOutput name _) -> name == from_port
            (SInput name _) -> name == from_port
            _ -> False


makeCells :: Integer -> [Component] -> [Connection] -> [Instance] -> [Cell]
makeCells bitCtr _ connections [] = []
makeCells bitCtr components connections (inst:instances) =
    Cell {
        cell_name = ins_name inst,
        cell_type = ins_cmp inst,
        -- add clk rst en ports, then for every port in this component type (..new info), 
        -- add connections as specified in connection list
        -- mapM_ print $ sys_connections $ head $ sys_subsystems expi'
        cell_connections = [
            CellConn "clk" [2],
            CellConn "rst" [3],
            CellConn "en" [4]
        ] 
    } : (trace ("\n\n----\n" ++ (unlines $ map show connections) ++ "\n----\n") $ makeCells bitCtr components connections instances)
    where
        component = head $ filter (\c -> cmp_name c == ins_cmp inst) components
        relevantConnections = filter relevant connections
        relevant (Connection (CID from_cmp from_port) (CID to_cmp to_port)) =
            from_cmp == ins_name inst || to_cmp == ins_name inst

