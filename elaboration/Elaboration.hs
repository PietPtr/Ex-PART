{-# LANGUAGE RecordWildCards #-}
module Elaboration where

import Types
import Repetition
import Multiconnection
import ElaborateConnection

import Data.Maybe


elaborate :: Design -> System
elaborate design = (elaborateSystem design (des_systree design)) {
        sys_topdata = TopData {
            top_cmbs = des_cmbs design,
            top_defs = des_defs design
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



class IsElement a where
    toElement :: a -> Element

instance IsElement Instance where
    toElement inst = Element {
            elem_name = ins_name,
            elem_size = ins_size,
            elem_coords = ins_coords,
            elem_iodefs = catMaybes $ map iso2io (cmp_isoStats ins_cmp),
            elem_implementation = InstanceImpl inst
        }
        where
            Instance {..} = inst

instance IsElement System where
    toElement system = Element {
            elem_name = sys_name,
            elem_size = sys_size,
            elem_coords = sys_coords,
            elem_iodefs = sys_iodefs,
            elem_implementation = SubsysImpl system
        }
        where
            System {..} = system
