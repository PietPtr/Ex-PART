module Postprocessing where

import Data.Aeson
import Debug.Trace
import qualified Data.Map as Map

import Types

type Netmap = Map.Map CID Net

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
    deriving (Show, Eq)

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
                mod_cells = makeCells components system netmap
            }
        nextBit = 1 + (last $ port_bits $ last $ mod_ports mod)
        netmap = nets components system nextBit portNetmap
            
        portNetmap = portsToNetmap (mod_ports mod) Map.empty

        portsToNetmap :: [Port] -> Netmap -> Netmap
        portsToNetmap [] map = map
        portsToNetmap (port:ports) netmap = portsToNetmap ports 
            $ Map.insert key value netmap
            where
                key = case port_direction port of
                    In -> cid
                    Out -> case filter isDriver (sys_connections system) of
                        ((Connection netCID _):_) -> netCID
                        [] -> error "Could not find driver."
                isDriver (Connection from to) = to == cid
                value = port_bits port
                cid = (CID "this" (port_name port))


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
nets :: [Component] -> System -> Integer -> Netmap -> Netmap
nets comps system bitCtr netmap = nets' comps system bitCtr (sys_connections system) netmap

nets' :: [Component] -> System -> Integer -> [Connection] -> Netmap -> Netmap
nets' _ _ _ [] map = map
nets' comps system bitCtr ((Connection from to):connections) netmap = 
    nets' comps system (bitCtr+netBitwidth) connections map'
    where
        map' = Map.insertWith seq from net netmap -- seq :: a -> b -> b ensures the original value is kept
        net = [bitCtr..(bitCtr+netBitwidth-1)]
        (CID from_elem_name from_port) = from

        netBitwidth = if from_elem_name == "this"
            then ioStatToBitWidth (io_statement system)
            else if isComponent from_elem_name
                then isoStatToBitwidth iso_statement
                else ioStatToBitWidth (io_statement subsystem)

        isComponent name = name `elem` (map (ins_name) $ sys_instances system)

        ---- vind de bitwidth voor een system IO
        io_statement sys = case filter findIOStat (sys_iodefs sys) of
            (x:_) -> x
            [] -> error $ "cannot find IO statement " ++ from_port ++ " in " ++ show (sys_iodefs sys)
        findIOStat stat = case stat of
            (Output name _) -> name == from_port
            (Input name _) -> name == from_port

        ---- vind de bitwidth voor een subsystem IO
        subsystem = case filter (\s -> sys_id s == from_elem_name) (sys_subsystems system) of
            (x:_) -> x
            [] -> error $ "cannot find subsystem " ++ from_elem_name

        ---- vind de bitwidth voor een bottom component IO
        -- in instances from_elem_name opzoeken
        component_name = ins_cmp $ case filter 
            (\i -> ins_name i == from_elem_name) 
            (sys_instances system) of
                (x:_) -> x
                [] -> error $ "cannot find component name " ++ from_elem_name ++ " in " ++ (show $ sys_instances system)
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

makeCells :: [Component] -> System -> Netmap -> [Cell]
makeCells components system netmap = makeInstanceCells components system netmap (sys_instances system)
    -- TODO: cells of subsystems

makeInstanceCells :: [Component] -> System -> Netmap -> [Instance] -> [Cell]
makeInstanceCells _ _ _ [] = []
makeInstanceCells components system netmap (inst:instances) =
    Cell {
        cell_name = ins_name inst,
        cell_type = ins_cmp inst,
        -- add clk rst en ports, then for every port in this component type (..new info), 
        -- add connections as specified in connection list
        cell_connections = [
            CellConn "clk" [2],
            CellConn "rst" [3],
            CellConn "en" [4]
        ] ++ portCells
    } : makeInstanceCells components system netmap instances
    where
        component = case filter (\c -> cmp_name c == ins_cmp inst) components of
            (x:_) -> x
            [] -> error $ "Could not find component for instance " ++ show inst
        relevantConnections = filter relevant connections
        relevant (Connection (CID from_cmp from_port) (CID to_cmp to_port)) =
            from_cmp == ins_name inst || to_cmp == ins_name inst
        
        connections = sys_connections system

        ports = filter isIO (cmp_isoStats component)
        isIO stat = case stat of
            (SState _ _ _) -> False
            _ -> True

        toCellConn port = CellConn name net
            where
                name = case port of
                    (SInput n _) -> n
                    (SOutput n _) -> n

                netCID = case filter lookupCID relevantConnections of
                    ((Connection cid _):_) -> cid
                    [] -> error $ "Cannot not find connection " ++ cmp_name component ++ "." ++ name
                lookupCID (Connection (CID from_elem from_port) (CID to_elem to_port)) = 
                    (from_port == name && from_elem == ins_name inst) || 
                    (to_port == name && to_elem == ins_name inst)

                net = Map.findWithDefault [] netCID netmap
                
        portCells = map toCellConn ports

