module Connections where

import Types

import Data.List
import Debug.Trace
import Data.Maybe

data Connective = Connective {
        con_from :: [String],
        con_to :: [String],
        con_done :: Bool
    } deriving Show

connectives :: System -> [Connective]
connectives system = map (connective system) (sys_connections system)


connective :: System -> Connection -> Connective
connective system (Connection from to) = Connective {
        con_from = [sys_name system, fromSys, fromPort],
        con_to = [sys_name system, toSys, toPort],
        con_done = done system toSys
    }
    where
        (CID fromSys fromPort) = from
        (CID toSys toPort) = to

refineUntilDone :: System -> Connective -> [Connective]
refineUntilDone system c@Connective{con_done=True} = [c]
refineUntilDone system c@Connective{con_done=False} = concat $ map (refineUntilDone system) next
    where
        next = refine system c

refine :: System -> Connective -> [Connective]
refine system connective = if con_done connective
    then [connective]
    else connectives
    where
        (toPort:toSys:_) = reverse (con_to connective)
        subsysNames = (map sys_name $ sys_subsystems system)
        toSubsys = case filter (\s -> sys_name s == toSys) (sys_subsystems system) of
            (x:_) -> x
            [] -> error $ "Connections.hs: Could not find system " ++ toSys ++ 
                " in subsystems of " ++ (sys_name system) ++ " (found :" ++ 
                (intercalate "," subsysNames) ++ ")"

        nextConnections = case catMaybes (map continue (sys_connections toSubsys)) of
            [] -> error $ "Connections.hs: Connection " ++ (show connective) ++ 
                    "is passed to next subsystem but never ends up at a component..."
            c -> c 

        continue conn = case conn of
            (Connection (CID "this" _) toCID) -> Just toCID
            _ -> Nothing

        connectives = map connective' nextConnections

        connective' :: CID -> Connective
        connective' (CID toSys' port) = connective {con_to = con_to', con_done = done system toSys'}
            where
                con_to' = init (con_to connective) ++ [toSys', port]

done :: System -> String -> Bool
done system toSys =  not $ toSys `elem` (map sys_name $ sys_subsystems system)


