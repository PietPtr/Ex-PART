module JSONBuilder where

import Data.Aeson

import Types
import Locations

writeLocationsJSON :: FilePath -> System -> IO ()
writeLocationsJSON basedir system = encodeFile (basedir ++ "/locations.json") $ 
    relToAbs (reduceAll $ allInstsWithCoords system) startPos system -- TODO: twee keer system?
    where
        startPos = case sys_coords system of
            (CConst x, CConst y) -> Pos x y
            _ -> error "Top-level coordinates must be constants." -- TODO: en 4 + 4 dan?
