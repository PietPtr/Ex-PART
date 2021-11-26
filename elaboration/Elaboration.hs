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
            map (\r -> (unrollRepetition r) (des_cmps design)) 
            (systr_repetitions systree)
        unrolledConnections = concat $ 
            map (unrollMulticonn fittedReps) (systr_multicons systree)
        allChainConns = allChainConnections fittedReps

        fittedReps = map fitRepetition (systr_repetitions systree)

        elems = 
            map toElement (systr_instances systree ++ unrolledInstances) ++
            map toElement (subsystems)
        
        subsystems = map (elaborateSystem design) (systr_subsystems systree)

        sysIO = systr_iodefs systree



