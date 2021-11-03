{-# LANGUAGE OverloadedStrings #-}

module Postprocessing where

import Data.Aeson
import Debug.Trace
import qualified Data.Map as Map
import Data.Text (pack, unpack)

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

data PortAndDir = PortAndDir String PortDir
    deriving Show

data Cell = Cell {
    cell_name :: String,
    cell_type :: String,
    cell_portDirections :: [PortAndDir], -- copy this from the type / module (Program => Component => ISOStats)
    cell_connections :: [CellConn]
    } deriving Show

--                       portid  "bits"
data CellConn = CellConn String [Integer]
    deriving Show

type Net = [Integer] -- directly a yosys "bits" entry

clockResetEnablePorts = [
        Port {
            port_name = "clk",
            port_direction = In,
            port_bits = [2]
        },
        Port {
            port_name = "rst",
            port_direction = In,
            port_bits = [3]
        },
        Port {
            port_name = "en",
            port_direction = In,
            port_bits = [4]
        }
    ]

instance ToJSON Module where
    toJSON mod = object [ pack (mod_name mod) .= object [
            "attributes" .= object (if mod_top mod then ["top" .= Number 1] else []),
            "ports" .= object (map (\p -> (pack $ port_name p) .= toJSON p) 
                (mod_ports mod ++ clockResetEnablePorts)),
            "cells" .= object (map (\c -> (pack $ cell_name c) .= toJSON c ) (mod_cells mod)),
            "netnames" .= object [
                "clk" .= object [
                    "bits" .= toJSON [Number 2],
                    "hide_name" .= Number 0,
                    "attributes" .= object []
                ]
            ]
        ]]
            

instance ToJSON Port where
    toJSON port = object [
            "direction" .= case port_direction port of
                In -> pack "input"
                Out -> pack "output",
            "bits" .= port_bits port
        ]

instance ToJSON Cell where
    toJSON cell = object [
            "hide_name" .= Number 0,
            "type" .= cell_type cell,
            "parameters" .= object [],
            "attributes" .= object [
                "module_not_derived" .= Number 1
            ],
            "port_directions" .= object (map dirs (cell_portDirections cell ++ clkrsten)),
            "connections" .= object (map conn (cell_connections cell))
        ]
        where
            conn (CellConn name bits) = (pack name) .= toJSON bits
            dirs (PortAndDir name dir) = (pack name) .= case dir of
                In -> pack "input"
                Out -> pack "output"
            clkrsten = map (\port -> (PortAndDir (port_name port) (port_direction port))) 
                clockResetEnablePorts

instance ToJSON CellConn where
    toJSON (CellConn name bits) = object [
            (pack name) .= bits
        ]

makeTopModule :: Program -> System -> [Module]
makeTopModule program system = makeModules True program system

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
                    Out -> findDriver system cid
                isDriver (Connection from to) = to == cid
                value = port_bits port
                cid = (CID "this" (port_name port))

findDriver :: System -> CID -> CID
findDriver system cid = case filter isDriver (sys_connections system) of
        ((Connection netCID _):_) -> netCID
        [] -> error $ "Could not find driver " ++ show cid ++ " in " ++ show (sys_connections system)
    where
        isDriver (Connection from to) = to == cid




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
        -- in instances from_elem_name opzoeken om het component te krijgen
        component = ins_cmp $ case filter 
            (\i -> ins_name i == from_elem_name) 
            (sys_instances system) of
                (x:_) -> x
                [] -> error $ "cannot find component name " ++ from_elem_name ++ " in " ++ (show $ sys_instances system)

        -- in isostats, vind port: de bitwidth van de from_port ophalen
        iso_statement = case filter findStat (cmp_isoStats component) of
            (x:_) -> x
            [] -> error $ "cannot find iso statement " ++ from_port
        findStat stat = case stat of
            (SOutput name _) -> name == from_port
            (SInput name _) -> name == from_port
            _ -> False

-- WARN: unique integers aren't sequential (but that probably does not matter)
makeCells :: [Component] -> System -> Netmap -> [Cell]
makeCells components system netmap = 
    -- makeInstanceCells components system netmap (sys_instances system) ++
    -- makeSubsystemCells system netmap (sys_subsystems system)
    makeCells' components system netmap (systemElems ++ instanceElems) 
    where
        systemElems = map sysElem (sys_subsystems system)
        instanceElems = map (instElem components) (sys_instances system)

-- abstracts over instance and system
data Element = Element {
        elem_name :: String,
        elem_type :: String,
        elem_io :: [IOStat],
        elem_ports :: [PortAndDir],
        elem_isSubsys :: Bool
    }


portWithDir :: IOStat -> PortAndDir
portWithDir stat = case stat of
    (Input n _) -> PortAndDir n In
    (Output n _) -> PortAndDir n Out

sysElem :: System -> Element
sysElem system = Element {
        elem_name = sys_id system,
        elem_type = sys_id system,
        elem_io = sys_iodefs system,
        elem_ports = map portWithDir $ sys_iodefs system,
        elem_isSubsys = True
    }

instElem :: [Component] -> Instance -> Element
instElem components inst = Element {
        elem_name = ins_name inst,
        elem_type = cmp_name $ ins_cmp inst,
        elem_io = elemio,
        elem_ports = map portWithDir elemio,
        elem_isSubsys = False
    }
    where
        elemio = map isoToIO ports
        component = ins_cmp inst

        ports = filter isIO (cmp_isoStats component)
        isIO stat = case stat of
            (SState _ _ _) -> False
            _ -> True

        isoToIO :: ISOStat -> IOStat
        isoToIO (SInput name t) = (Input name t)
        isoToIO (SOutput name t) = (Output name t)
        isoToIO (SState _ _ _) = error "Cannot convert state to IO"


makeCells' :: [Component] -> System -> Netmap -> [Element] -> [Cell]
makeCells' _ _ _ [] = []
makeCells' components system netmap (elem:elements) =
    Cell {
        cell_name = elem_type elem ++ infix_name ++ elem_name elem,
        cell_type = elem_type elem,
        -- add clk rst en ports, then for every port in this component type (..new info), 
        -- add connections as specified in connection list
        cell_portDirections = elem_ports elem,
        cell_connections = [
            CellConn "clk" [2],
            CellConn "rst" [3],
            CellConn "en" [4]
        ] ++ portCells
    } : makeCells' components system netmap elements
    where
        infix_name = if elem_isSubsys elem
            then "-system-"
            else "-instance-"

        relevantConnections = filter relevant connections
        relevant (Connection (CID from_cmp from_port) (CID to_cmp to_port)) =
            from_cmp == elem_name elem || to_cmp == elem_name elem
        
        connections = sys_connections system

        toCellConn port = CellConn name net
            where
                name = case port of
                    (Input n _) -> n
                    (Output n _) -> n

                netCID = case filter lookupCID relevantConnections of
                    ((Connection cid _):_) -> Just cid
                    [] -> Nothing -- error $ "Cannot find connection `" ++ name ++ "` in `" ++ elem_name elem ++ "`"
                lookupCID (Connection (CID from_elem from_port) (CID to_elem to_port)) = 
                    (from_port == name && from_elem == elem_name elem) || 
                    (to_port == name && to_elem == elem_name elem)

                net = case netCID of
                    Just cid -> Map.findWithDefault [] cid netmap
                    Nothing -> [] -- TODO: can we just leave stuff disconnected?
                
        portCells = map toCellConn (elem_io elem)





test =[
    Module {
        mod_name = "system", mod_top = True, mod_ports = [Port {port_name = "result", port_direction = Out, port_bits = [5,6,7,8,9,10,11,12]},Port {port_name = "setting", port_direction = In, port_bits = [13,14,15,16,17,18,19,20,21]}], mod_cells = [Cell {cell_name = "collatzer", cell_type = "collatzer", cell_portDirections = [PortAndDir "val_out" Out,PortAndDir "val_in" In], cell_connections = [CellConn "clk" [2],CellConn "rst" [3],CellConn "en" [4],CellConn "val_out" [39,40,41,42,43,44,45,46],CellConn "val_in" [5,6,7,8,9,10,11,12]]},Cell {cell_name = "controller", cell_type = "control", cell_portDirections = [PortAndDir "next_val" In,PortAndDir "set_val" In,PortAndDir "result_value" Out], cell_connections = [CellConn "clk" [2],CellConn "rst" [3],CellConn "en" [4],CellConn "next_val" [39,40,41,42,43,44,45,46],CellConn "set_val" [13,14,15,16,17,18,19,20,21],CellConn "result_value" [5,6,7,8,9,10,11,12]]}]},Module {mod_name = "collatzer", mod_top = False, mod_ports = [Port {port_name = "val_out", port_direction = Out, port_bits = [5,6,7,8,9,10,11,12]},Port {port_name = "val_in", port_direction = In, port_bits = [13,14,15,16,17,18,19,20]}], mod_cells = [Cell {cell_name = "merger", cell_type = "merger", cell_portDirections = [PortAndDir "vo" In,PortAndDir "ve" In,PortAndDir "res" Out], cell_connections = [CellConn "clk" [2],CellConn "rst" [3],CellConn "en" [4],CellConn "vo" [38,39,40,41,42,43,44,45,46],CellConn "ve" [29,30,31,32,33,34,35,36,37],CellConn "res" [5,6,7,8,9,10,11,12]]},Cell {cell_name = "onEven", cell_type = "onEven", cell_portDirections = [PortAndDir "val" In,PortAndDir "res" Out], cell_connections = [CellConn "clk" [2],CellConn "rst" [3],CellConn "en" [4],CellConn "val" [47,48,49,50,51,52,53,54,55],CellConn "res" [29,30,31,32,33,34,35,36,37]]},Cell {cell_name = "onOdd", cell_type = "onOdd", cell_portDirections = [PortAndDir "val" In,PortAndDir "res" Out], cell_connections = [CellConn "clk" [2],CellConn "rst" [3],CellConn "en" [4],CellConn "val" [56,57,58,59,60,61,62,63,64],CellConn "res" [38,39,40,41,42,43,44,45,46]]},Cell {cell_name = "router", cell_type = "router", cell_portDirections = [PortAndDir "val" In,PortAndDir "odd" Out,PortAndDir "even" Out], cell_connections = [CellConn "clk" [2],CellConn "rst" [3],CellConn "en" [4],CellConn "val" [13,14,15,16,17,18,19,20],CellConn "odd" [56,57,58,59,60,61,62,63,64],CellConn "even" [47,48,49,50,51,52,53,54,55]]}]}]
