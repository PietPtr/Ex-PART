module Postprocessing where

import Data.Aeson

-- definieer een mooi datatype, bouw bestaande System/Program om hiernaartoe, het nice makend
-- definieer dan een toJSON hiervoor, verander het naar een Value, en prop het in de bestaande JSON
data Module = Module {
    mod_top :: Bool, -- all attributes...
    mod_ports :: [Port],
    mod_cells :: [Cell] 
}

data Port = Port {
    port_name :: String,
    port_direction :: PortDir,
    port_bits :: [Integer]
}

data Cell = Cell {
    cell_name :: String,
    cell_type :: String,
    -- cell_portDirections :: [Port] -- copy this from the type / module (Program => Component => ISOStats)
    cell_connections :: [Integer]
}

parseTest :: IO (Maybe Value)
parseTest = decodeFileStrict "testenv/synthesized.json"

