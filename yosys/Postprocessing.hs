{-# LANGUAGE OverloadedStrings #-}

module Postprocessing where

import Data.Aeson
import qualified Data.Map as Map
import Data.Text (pack)
import Data.List
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe

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

clockResetEnablePorts :: [Port]
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
            "bits" .= (map fix01 $ port_bits port)
        ]
        where
            fix01 :: Integer -> Value
            fix01 0 = String "0"
            fix01 1 = String "1"
            fix01 c = Number (fromIntegral c)

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

makeConstModules :: Program -> System -> [Module]
makeConstModules program system = case driver of
    Just driver -> constDriverModule driver : (makeConstModules program system')
    Nothing -> []
    where
        (driver, drivers) = case sys_constantDrivers system of
            (d:ds) -> (Just d, Just ds)
            [] -> (Nothing, Nothing)
        system' = system {sys_constantDrivers=(fromJust drivers)} -- only used in the just case, type system cannot prove this tho


-- TODO (elab): in the typing refactor, can the bitwidth not be annotated somewhere? saves a crapton of lookups
findCIDBitwidth :: System -> CID -> Integer
findCIDBitwidth system cid = isoStatToBitwidth iso_statement
    where
        (CID instName portName) = cid
        component = ins_cmp $ case filter 
            (\i -> ins_name i == instName) 
            (sys_instances system) of
                (x:_) -> x
                [] -> error $ "Postprocessing.hs: cannot find component name " ++ instName ++ "."

        iso_statement = case filter findStat (cmp_isoStats component) of
            (x:_) -> x
            [] -> error $ "Postprocessing.hs: cannot find iso statement " ++ portName

        findStat stat = case stat of
            (SOutput name _) -> name == portName
            (SInput name _) -> name == portName
            _ -> False


makeModules :: Bool -> Program -> System -> [Module]
makeModules isTop p@(Program _ _ components) system = mod : 
    (concat $ map (makeModules False p) (sys_subsystems system))
    where
        mod = Module {
                mod_name = sys_id system,
                mod_top = isTop,
                mod_ports = makePorts 5 $ sys_iodefs system',
                mod_cells = makeCells components system' netmap
            }
        nextBit = 1 + (last $ port_bits $ last $ mod_ports mod)
        netmap = nets components system' nextBit portNetmap
            
        portNetmap = portsToNetmap (mod_ports mod) Map.empty

        portsToNetmap :: [Port] -> Netmap -> Netmap
        portsToNetmap [] map = map
        portsToNetmap (port:ports) netmap = portsToNetmap ports 
            $ Map.insert key value netmap
            where
                key = case port_direction port of
                    In -> cid
                    Out -> findDriver system' cid
                    
                value = port_bits port
                cid = (CID "this" (port_name port))

        -- Add instances and connections to this system for the constant drivers
        system' = system {
            sys_instances=sys_instances system ++ cdInstances,
            sys_connections=sys_connections system ++ cdConns
        }

        cdConns = map cdToConn (sys_constantDrivers system)
        cdInstances = map cdToInstance (sys_constantDrivers system)

        cdToConn :: ConstantDriver -> Connection
        cdToConn (ConstantDriver value cid) = (Connection constCID cid)
            where
                constCID = (CID elemName "out")
                elemName = constModuleName (ConstantDriver value cid)

        cdToInstance :: ConstantDriver -> Instance
        cdToInstance (ConstantDriver value cid) = Instance {
                ins_name = elemName,
                ins_cmp = Component elemName [] [SOutput "out" "CONSTANT"] "",
                ins_size = (0, 0),
                ins_coords = (CConst 0, CConst 0),
                ins_args = []
            }
            where
                elemName = constModuleName (ConstantDriver value cid)


findDriver :: System -> CID -> CID
findDriver system cid = case filter isDriver (sys_connections system) of
        ((Connection netCID _):_) -> netCID
        [] -> error $ "Postprocessing.hs: Could not find driver " ++ show cid ++ " in " ++ show (sys_connections system)
    where
        isDriver (Connection _ to) = to == cid




-- bitCtr counts which netnumber is free
makePorts :: Integer -> [IOStat] -> [Port]
makePorts _ [] = []
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

-- TODO (lowprio): I _think_ we can just, instead of netnames, initialize constants here, so in the synthesized.json it will end up as "0" and "1" instead of nets, and saves generating const modules (doesn't seem to affect performance results.)
-- TODO (elab): why is bitwidth determination here? could be elsewhere?
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
            else if "const_" `isPrefixOf` from_elem_name
                -- TODO (elab): this bitwidth determination is correct, however all nets not supplied by the constant module must be set to zero, otherwise not all bits have drivers.
                then findCIDBitwidth system to -- assume the user connected the constant driver correctly
                else if isComponent from_elem_name
                    then findCIDBitwidth system from
                    else  ioStatToBitWidth (io_statement subsystem)

        isComponent name = name `elem` (map (ins_name) $ sys_instances system)

        ---- vind de bitwidth voor een system IO
        io_statement sys = case filter findIOStat (sys_iodefs sys) of
            (x:_) -> x
            [] -> error $ "Postprocessing.hs: cannot find IO statement " ++ from_port ++ " in " ++ show (sys_iodefs sys)
        findIOStat stat = case stat of
            (Output name _) -> name == from_port
            (Input name _) -> name == from_port

        ---- vind de bitwidth voor een subsystem IO
        subsystem = case filter (\s -> sys_id s == from_elem_name) (sys_subsystems system) of
            (x:_) -> x
            [] -> error $ "Postprocessing.hs: cannot find subsystem " ++ from_elem_name


-- WARN: unique integers aren't sequential (but that probably does not matter)
makeCells :: [Component] -> System -> Netmap -> [Cell]
makeCells components system netmap = 
    makeCells' components system netmap (systemElems ++ instanceElems) 
    where
        systemElems = map sysElem (sys_subsystems system)
        instanceElems = map instElem (sys_instances system)

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

instElem :: Instance -> Element
instElem inst = Element {
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
        isoToIO (SState _ _ _) = error "Postprocessing.hs: Cannot convert state to IO"



constDriverModule :: ConstantDriver -> Module
constDriverModule (ConstantDriver value cid) = Module 
    { mod_name = constModuleName (ConstantDriver value cid)
    , mod_top = False
    , mod_ports = clockResetEnablePorts ++ [outPort]
    , mod_cells = [] }
    where
        outPort = Port {
            port_name = "out",
            port_direction = Out,
            port_bits = toBinary value 
        }

constModuleName :: ConstantDriver -> String
constModuleName (ConstantDriver value _) = "const_" ++ value

-- produces a list of ones and zero for the value given in the string
-- only implements 10 integers
toBinary :: String -> [Integer]
toBinary value = map (\c -> read [c]) str
    where
        int = read value :: Integer
        str = showIntAtBase 2 intToDigit int ""


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
        relevant (Connection (CID from_cmp _) (CID to_cmp _)) =
            from_cmp == elem_name elem || to_cmp == elem_name elem
        
        connections = sys_connections system

        toCellConn port = CellConn name net
            where
                name = case port of
                    (Input n _) -> n
                    (Output n _) -> n

                netCID = case filter lookupCID relevantConnections of
                    ((Connection cid _):_) -> Just cid
                    [] -> Nothing 
                lookupCID (Connection (CID from_elem from_port) (CID to_elem to_port)) = 
                    (from_port == name && from_elem == elem_name elem) || 
                    (to_port == name && to_elem == elem_name elem)

                net = case netCID of
                    Just cid -> case Map.lookup cid netmap of
                        Just n -> n
                        Nothing -> error $ "Postprocessing.hs: cannot find net for cid in netmap:\n" ++
                            (show cid) ++ "\n" ++ (show netmap)
                    Nothing -> error $ "Postprocessing.hs: No net found, something is disconnected...\n" ++
                        (show port) ++ "\n" ++ (show relevantConnections)

                
        portCells = map toCellConn (elem_io elem)


