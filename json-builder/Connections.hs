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
        con_from = [sys_id system, fromSys, fromPort],
        con_to = [sys_id system, toSys, toPort],
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
        subsysNames = (map sys_id $ sys_subsystems system)
        toSubsys = case filter (\s -> sys_id s == toSys) (sys_subsystems system) of
            (x:_) -> x
            [] -> error $ "Could not find system " ++ toSys ++ 
                " in subsystems of " ++ (sys_id system) ++ " (found :" ++ 
                (intercalate "," subsysNames) ++ ")"

        nextConnections = case catMaybes (map continue (sys_connections toSubsys)) of
            [] -> error $ "Connection " ++ (show connective) ++ 
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
done system toSys =  not $ toSys `elem` (map sys_id $ sys_subsystems system)




conn = Connection (CID "controller" "result_value") (CID "collatzer" "val_in")
coll = System {sys_flattened = False, sys_id = "system", sys_size = (6,6), sys_coords = (CConst 2,CConst 2), sys_iodefs = [Output "result" "Value",Input "setting" "Maybe Value"], sys_instances = [Instance {ins_name = "controller", ins_cmp = Component {cmp_name = "control", cmp_args = [], cmp_isoStats = [SInput "next_val" "Value",SInput "set_val" "Maybe Value",SState "last_val" (Constant 0) "Value",SOutput "result_value" "Value"], cmp_where = "last_val' = case set_val of\n        Just new_value -> new_value\n        Nothing -> next_val\n\n    result_value = last_val\n"}, ins_args = [], ins_size = (6,1), ins_coords = (CConst 0,CConst 0)}], sys_connections = [Connection (CID "controller" "result_value") (CID "this" "result"),Connection (CID "this" "setting") (CID "controller" "set_val"),Connection (CID "collatzer" "val_out") (CID "controller" "next_val"),Connection (CID "controller" "result_value") (CID "collatzer" "val_in")], sys_repetitions = [], sys_subsystems = [System {sys_flattened = False, sys_id = "collatzer", sys_size = (6,4), sys_coords = (CConst 0,CHeight "controller"), sys_iodefs = [Output "val_out" "Value",Input "val_in" "Value"], sys_instances = [Instance {ins_name = "merger", ins_cmp = Component {cmp_name = "merger", cmp_args = [], cmp_isoStats = [SInput "vo" "Maybe Value",SInput "ve" "Maybe Value",SOutput "res" "Value"], cmp_where = "res = case vo of\n        Just v -> v\n        Nothing -> case ve of\n            Just v -> v\n            Nothing -> 0\n"}, ins_args = [], ins_size = (1,4), ins_coords = (CAdd (CX "onOdd") (CWidth "onOdd"),CConst 0)},Instance {ins_name = "onEven", ins_cmp = Component {cmp_name = "onEven", cmp_args = [], cmp_isoStats = [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"], cmp_where = "res = case val of\n        Just v -> Just $ v >>> 1\n        Nothing -> Nothing\n"}, ins_args = [], ins_size = (4,2), ins_coords = (CWidth "router",CHeight "onOdd")},Instance {ins_name = "onOdd", ins_cmp = Component {cmp_name = "onOdd", cmp_args = [], cmp_isoStats = [SInput "val" "Maybe Value",SOutput "res" "Maybe Value"], cmp_where = "res = case val of\n        Just v -> Just $ (v <<< 1 + v) + 1\n        Nothing -> Nothing\n"}, ins_args = [], ins_size = (4,2), ins_coords = (CWidth "router",CConst 0)},Instance {ins_name = "router", ins_cmp = Component {cmp_name = "router", cmp_args = [], cmp_isoStats = [SInput "val" "Value",SOutput "odd" "Maybe Value",SOutput "even" "Maybe Value"], cmp_where = "even = if testBit val 0 then Nothing else Just val\n    odd  = if testBit val 0 then Just val else Nothing\n"}, ins_args = [], ins_size = (1,4), ins_coords = (CConst 0,CConst 0)}], sys_connections = [Connection (CID "merger" "res") (CID "this" "val_out"),Connection (CID "onEven" "res") (CID "merger" "ve"),Connection (CID "onOdd" "res") (CID "merger" "vo"),Connection (CID "router" "even") (CID "onEven" "val"),Connection (CID "router" "odd") (CID "onOdd" "val"),Connection (CID "this" "val_in") (CID "router" "val")], sys_repetitions = [], sys_subsystems = []}]}