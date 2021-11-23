module ElaborateConnection where

import Types

elaborateConnection :: [Element] -> [IOStat] -> Connection -> Connection'
elaborateConnection elems sysIO (Connection from to) = if fromType == toType
    then (Connection' from to bitwidth)
    else error $ "ElaborateConnection.hs: Cannot connect ports " ++ (show from) ++ "->" ++ (show to) ++ 
        " as they have differing types: " ++ fromType ++ "->" ++ toType ++ "."
    where
        (CID from_elem from_port) = from
        (CID to_elem to_port) = to
        
        bitwidth = typeToBitwidth toType

        fromType = portType (findIOStatByName from_elem) from_port
        toType = portType (findIOStatByName to_elem) to_port

        findIOStatByName :: String -> [IOStat]
        findIOStatByName name = if name == "this"
            then sysIO
            else case filter ((name ==) . elem_name) elems of
                [e] -> elem_iodefs e
                [] -> error $ "ElaborateConnection.hs: Cannot find element with name " 
                        ++ name ++ " in " ++ show elemnames
                _ -> error $ "ElaborateConnection.hs: Found several components with name " 
                        ++ name ++ " in " ++ show elemnames
            where
                elemnames = map elem_name elems

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
