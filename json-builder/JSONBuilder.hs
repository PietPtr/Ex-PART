module JSONBuilder where

import Data.Aeson

import Types
import Locations

writeLocationsJSON :: FilePath -> System -> IO ()
writeLocationsJSON basedir system = encodeFile (basedir ++ "/locations.json") $ 
    relToAbs (reduceAll $ allInstsWithCoords system) startPos system
    where
        startPos = if isConstExpr x && isConstExpr y
            then Pos (reduceCoordExpr [] x) (reduceCoordExpr [] y)
            else error "Top-level coordinates must be constants."
        (x, y) = sys_coords system

        isConstExpr :: CoordExpr -> Bool
        isConstExpr (CConst _) = True
        isConstExpr (CAdd left right) = isConstExpr left && isConstExpr right
        isConstExpr _ = False