{-# LANGUAGE RecordWildCards #-}
module Elaboration where

import Types
import Repetition
import Multiconnection
import ElaborateConnection
import Debug.Trace


elaborate :: Design -> System
elaborate design = (elaborateSystem design (des_systree design)) {
        sys_topdata = TopData {
            top_cmbs = des_cmbs design,
            top_defs = des_defs design,
            top_cmps = des_cmps design
        }
    }


elaborateSystem :: Design -> SystemTree -> System
elaborateSystem design systree = System {
        sys_name = systr_name systree,
        sys_type = systr_type systree,
        sys_topdata = NotTop,
        sys_size = systr_size systree,
        sys_coords = systr_coords systree,
        sys_iodefs = sysIO,
        sys_elems = elems,
        sys_constantDrivers = systr_constantDrivers systree,
        sys_connections = map (elaborateConnection elems sysIO) $
            systr_connections systree ++ 
            unrolledConnections ++ 
            allChainConns
    }
    where
        unrolledInstances = concat $ 
            map (\r -> (unrollRepetition r) elemsWithoutReps) 
            (systr_repetitions systree)
        unrolledConnections = concat $ 
            map (unrollMulticonn fittedReps) (systr_multicons systree)
        allChainConns = allChainConnections fittedReps

        fittedReps = map fitRepetition (systr_repetitions systree)

        elems = unrolledInstances ++ elemsWithoutReps

        elemsWithoutReps = 
            map toElement (componentInstances) ++
            map toElement (subsystems)

        componentInstances = filter ins_isCmpInstance (systr_instances systree)
        systemInstances = filter (not . ins_isCmpInstance) (systr_instances systree)

        systemInstanceSystems = map 
            (\si -> findSubsysDef (systr_subsystems systree) si) systemInstances

        subsystems = map (elaborateSystem design) 
            (systr_subsystems systree ++ systemInstanceSystems)

        sysIO = systr_iodefs systree



findSubsysDef :: [SystemTree] -> Instance -> SystemTree
findSubsysDef systems inst = case sys of
    Just system -> system { 
            systr_name = name, 
            systr_size = sins_size,
            systr_coords = sins_coords
        }
    Nothing -> case subsystems of
        [] -> error $ "Cannot find system " ++ name ++ " in this scope." ++ show (map systr_name systems)
        systems -> findSubsysDef systems inst
    where
        subsystems = concat $ map systr_subsystems systems
        sys = case filter (\s -> systr_type s == sysType) systems of 
            (x:_) -> Just x
            _ -> Nothing

        sysType = sins_sysname
        name = sins_name

        SysInstance{..} = inst