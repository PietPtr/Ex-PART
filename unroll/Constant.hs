module Constant where

import Types
import Data.List

unrollConstDrivers :: [Connection] -> [Component] -> ([Connection], [Component])
unrollConstDrivers conns cmps = (constConns, uniqueNewComponents)
    where
        constantDrivers = filter isConstDriver conns
        regularConns = filter (not . isConstDriver) conns
        isConstDriver conn = case conn of
                (Connection (ConstantDriver _) (CID _ _)) -> True
                (Connection (CID _ _)          (CID _ _)) -> False

        newConnsAndCmps = map (unrollConstDriver cmps) constantDrivers
        constConns = map fst newConnsAndCmps

        equalCmpByName cmp1 cmp2 = (cmp_name cmp1) == (cmp_name cmp2)
        uniqueNewComponents = nubBy equalCmpByName (map snd newConnsAndCmps)

        
-- bug: ugh er moet ook nog een instance toegevoegd natuurlijk
unrollConstDriver :: [Component] -> Connection -> (Connection, Component)
unrollConstDriver cmps conn = unrollConstDriver' conn cmp
    where
        (Connection _ (CID cmpName _)) = conn
        cmp = case filter (\cmp -> cmp_name cmp == cmpName) cmps of
            (x:_) -> x
            _ -> error $ "Constant.hs: Cannot find component with name " ++ cmpName

-- We assume the programmer gives a constant driver of the same type as the port of the
-- component it drives. Otherwise at least the Clash simulation is going to give errors :P
-- TODO: can only constant-drive components now (is that a big deal?)
-- TODO: bigger refactor that there is no difference between components and systems anymore?
unrollConstDriver' :: Connection -> Component -> (Connection, Component)
unrollConstDriver' conn destCmp = (conn', cmp)
    where
        (Connection (ConstantDriver constant) toCID) = conn
        (CID _ port) = toCID
        portType = case filter (findPort port) (cmp_isoStats destCmp) of
            ((SInput _ t):_) -> t
            _ -> error $ "Constant.hs: cannot find port " ++ port ++ " in component " ++ (cmp_name destCmp)

        findPort portName isoStat = case isoStat of
            (SInput name _) -> name == portName
            _ -> False

        constDriverID = "constant_" ++ identifierify constant
        cmp = Component {
                cmp_name = constDriverID,
                cmp_args = [],
                cmp_isoStats = [SOutput "out" portType],
                cmp_where = "TODO"
            }
        conn' = Connection (CID constDriverID "out") toCID


 
identifierify :: String -> String
identifierify s = concat $ map repl s
    where
        repl c
            | c `elem` (['0'..'9']++['a'..'z']++['A'..'Z']) = [c]
            | c `elem` ['(', ')'] = ""
            | otherwise = "_"

