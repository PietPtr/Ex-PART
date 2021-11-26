module ElaborateConnection where

import Types
import Debug.Trace

elaborateConnection :: [Element] -> [IOStat] -> Connection -> Connection'
elaborateConnection elems sysIO (Connection from to) = if fromType == toType
    then (Connection' from to bitwidth)
    else error $ "ElaborateConnection.hs: Cannot connect ports " ++ (show from) ++ "->" ++ (show to) ++ 
        " as they have differing types: " ++ fromType ++ "->" ++ toType ++ "."
    where
        (CID from_elem from_port) = from
        (CID to_elem to_port) = to
        
        bitwidth = typeToBitwidth toType

        fromType = portType (findIOStatByName elems sysIO from_elem) from_port
        toType = portType (findIOStatByName elems sysIO to_elem) to_port

portType :: [IOStat] -> String -> String
portType iostats portName = case filter (findIOStat) iostats of
    [s] -> case s of
        (Output _ t) -> t
        (Input _ t) -> t
    [] -> error $ "ElaborateConnection.hs: Cannot find port with name "
            ++ portName ++ " in " ++ show iostats
    _ -> error $ "ElaborateConnection.hs: Found several ports with name "
            ++ portName ++ " in " ++ show iostats
    where
        findIOStat stat = case stat of
            (Output name _) -> name == portName
            (Input name _) -> name == portName

-- TODO (lowprio): can be prettier if the IOStats are put in a hashmap or smth
findIOStatByName :: [Element] -> [IOStat] -> String -> [IOStat]
findIOStatByName elems sysIO name = if name == "this"
    then sysIO
    else case filter ((name ==) . elem_name) elems of
        [e] -> elem_iodefs e
        [] -> error $ "ElaborateConnection.hs: Cannot find element with name " 
                ++ name ++ " in " ++ show elemnames
        _ -> error $ "ElaborateConnection.hs: Found several components with name " 
                ++ name ++ " in " ++ show elemnames
    where
        elemnames = map elem_name elems


portBitwidth :: System -> CID -> Integer
portBitwidth system (CID compName portName) = typeToBitwidth porttype
    where
        elems = sys_elems system
        sysIO = sys_iodefs system

        iostats = findIOStatByName elems sysIO compName
        porttype = portType iostats portName
