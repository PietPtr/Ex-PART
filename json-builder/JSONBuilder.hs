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
            then Pos (reduceLayoutExpr [] x) (reduceLayoutExpr [] y)
            else error "JSONBuilder.hs: Top-level coordinates must be constants."
        (x, y) = sys_coords system

        isConstExpr :: LayoutExpr -> Bool
        isConstExpr (CConst _) = True
        isConstExpr (CAdd left right) = isConstExpr left && isConstExpr right
        isConstExpr (CSub left right) = isConstExpr left && isConstExpr right
        isConstExpr _ = False

        -- TODO (lowprio): the cycle checker is a _bit_ too aggressive, since it checks for any property being referenced, but it rejects some solvable instances.
        check = if hasCycle graph
            -- TODO: allow self reference (don't count length one cycles)
            then error "JSONBuilder.hs: expi file contains a cyclic coordinate definition, cannot generate location JSON."
            else True
            where
                graph = toGraph $ toGraphList instances
