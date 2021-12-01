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
            top_defs = des_defs design,
            top_cmps = des_cmps design
        }
    }


elaborateSystem :: Design -> SystemTree -> System
elaborateSystem design systree = System {
        sys_unplaced = systr_unplaced systree,
        sys_name = systr_name systree,
        sys_type = systr_type systree,
        sys_topdata = NotTop,
        sys_size = systr_size systree,
        sys_coords = systr_coords systree,
        sys_iodefs = sysIO,
        sys_elems = elems,
        sys_allElems = unrolledInstances ++ elemsWithoutReps,
        sys_constantDrivers = systr_constantDrivers systree,
        sys_connections = map (elaborateConnection elems sysIO) $
            systr_connections systree ++ 
            unrolledConnections ++ 
            allChainConns
    }
    where
        unrolledInstances = concat $ 
            map (\r -> (unrollRepetition r) (des_cmps design) elemsWithoutReps) 
            (systr_repetitions systree)
        unrolledConnections = concat $ 
            map (unrollMulticonn fittedReps) (systr_multicons systree)
        allChainConns = allChainConnections fittedReps

        fittedReps = map fitRepetition (systr_repetitions systree)

        elems = filter (not . elem_unplaced) $ 
            unrolledInstances ++ elemsWithoutReps

        elemsWithoutReps = 
            map toElement (componentInstances) ++
            map toElement subsystems

        componentInstances = filter ins_isCmpInstance (systr_instances systree)
        systemInstances = filter (not . ins_isCmpInstance) (systr_instances systree)

        systemInstanceSystrees = map 
            (\si -> findSubsysDef (systr_subsystems systree) si) systemInstances

        subsystems = map (elaborateSystem design) systrees
        
        systrees = (systr_subsystems systree ++ systemInstanceSystrees)

        sysIO = systr_iodefs systree



findSubsysDef :: [SystemTree] -> Instance -> SystemTree
findSubsysDef systrees inst = case sys of
    Just systree -> systree { 
            systr_name = name, 
            systr_size = sins_size,
            systr_coords = sins_coords
        }
    Nothing -> case subsystrees of
        [] -> error $ "Cannot find system " ++ name ++ " in this scope." ++ show (map systr_name systrees)
        systrees -> findSubsysDef systrees inst
    where
        subsystrees = concat $ map systr_subsystems systrees
        sys = case filter (\s -> systr_type s == sysType) systrees of 
            (x:_) -> Just x
            _ -> Nothing

        sysType = sins_sysname
        name = sins_name

        SysInstance{..} = inst