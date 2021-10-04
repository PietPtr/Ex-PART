module JSONBuilder where

import Data.Aeson

import Types
import Locations

writeLocationsJSON :: System -> IO ()
writeLocationsJSON system = do
    if check
        then encodeFile ("locations.json") json
        else putStrLn ("Error in Location JSON writing.")
    where
        json = relToAbs (reduceAll instances) startPos system

        instances = allInstsWithCoords system

        startPos = if isConstExpr x && isConstExpr y
            then Pos (reduceCoordExpr [] x) (reduceCoordExpr [] y)
            else error "Top-level coordinates must be constants."
        (x, y) = sys_coords system

        isConstExpr :: CoordExpr -> Bool
        isConstExpr (CConst _) = True
        isConstExpr (CAdd left right) = isConstExpr left && isConstExpr right
        isConstExpr _ = False

        check = if hasCycle graph
            then error "expi file contains a cyclic coordinate definition, cannot generate location JSON."
            else True
            where
                graph = toGraph $ toGraphList instances