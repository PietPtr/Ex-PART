module Postprocessing where

import Types
import Data.Aeson

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

data CellConn = CellConn String [Integer]
    deriving Show

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
                CellConn "odd" [ {- bepaalde nets -} ],
                CellConn "even" [ {--} ]
            ]
        }
        -- en nog 3 cells voor de onOdd, onEven, en merger
    ]
}

parseTest :: IO (Maybe Value)
parseTest = decodeFileStrict "testenv/synthesized.json"

makeTopModule :: System -> [Module]
makeTopModule system = makeModules True system

-- TODO: beware that it may be necessary to introduce a bookkeeping datastructure, create a record for this.

makeModules :: Bool -> System -> [Module]
makeModules isTop system = mod : (concat $ map (makeModules False) (sys_subsystems system))
    where
        mod = Module {
                mod_name = sys_id system,
                mod_top = isTop,
                mod_ports = makePorts 5 $ sys_iodefs system,
                mod_cells = makeCells (sys_connections system) (sys_instances system)
            }

-- bitctr counts which netnumber is free
makePorts :: Integer -> [IOStat] -> [Port]
makePorts bitctr [] = []
makePorts bitctr (stat:stats) = 
    Port {
        port_name = name,
        port_direction = direction,
        port_bits = [bitctr..(bitctr+bitwidth-1)]
    } : makePorts (bitctr+bitwidth) stats
    where
        (direction, name, t) = case stat of
            (Input iname it) -> (In, iname, it)
            (Output oname ot) -> (Out, oname, ot)
        bitwidth = typeToBitwidth t


makeCells :: [Connection] -> [Instance] -> [Cell]
makeCells connections [] = []
makeCells connections (inst:instances) =
    Cell {
        cell_name = ins_name inst,
        cell_type = ins_cmp inst,
        -- add clk rst en ports, then for every port in this component type (..new info), 
        -- add connections as specified in connection list
        -- mapM_ print $ sys_connections $ head $ sys_subsystems expi'
        cell_connections = [] 
    } : makeCells connections instances
    -- where
