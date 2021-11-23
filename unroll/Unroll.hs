module Unroll where

import Types
import Chain
import Repeat
import Multiconnection

-- TODO (elab): Turn this in the elaboration phase
-- Ensures all repetitions and multiconnections are unrolled into instances and connections
-- TODO (elab): split up data types such that this guarantee is type-checkable?
unroll :: Program -> System -> (Program, System)
unroll program system = (program', system')
    where
        system' = unrollSystem program system
        
        -- TODO (elab): delete components not used in expi
        program' = program


unrollSystem :: Program -> System -> System
unrollSystem program system = system {
        sys_instances = 
            sys_instances system ++ 
            unrolledInstances,
        sys_connections = 
            sys_connections system ++ 
            unrolledConnections ++ 
            allChainConnections,
        sys_subsystems = map (unrollSystem program) (sys_subsystems system)
    }
    where
        unrolledInstances = concat $ map (\r -> (unrollRepetition r) (prg_cmps program)) (sys_repetitions system)
        unrolledConnections = concat $ 
            map (unrollMulticonn $ sys_repetitions system) (sys_multicons system)
        allChainConnections = concat $
            map chainConnections ([ x | x@(Chain {}) <- sys_repetitions system])



unrollRepetition :: Repetition -> ([Component] -> [Instance])
unrollRepetition rep = case rep of
    Chain {} -> (\cs -> unrollChain cs rep)
    Repeat {} -> (\cs -> unrollRepeat cs rep)

