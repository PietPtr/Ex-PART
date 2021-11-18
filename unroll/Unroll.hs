module Unroll where

import Types
import Chain
import Repeat
import Multiconnection
import Constant -- cancelled, but keeping in the parsing features if time is left over

-- Ensures all repetitions and multiconnections are unrolled into instances and connections
-- TODO: split up data types such that this guarantee is type-checkable?
unroll :: Program -> System -> (Program, System)
unroll program system = (program', system')
    where
        system' = system {
            sys_instances = 
                sys_instances system ++ 
                unrolledInstances,
            sys_connections = 
                sys_connections system ++ 
                unrolledConnections ++ 
                allChainConnections 
                -- constConns
        }
        program' = program {
            prg_cmps = prg_cmps program -- ++ constCmps
        }

        unrolledInstances = concat $ map (\r -> (unrollRepetition r) (prg_cmps program)) (sys_repetitions system)
        unrolledConnections = concat $ 
            map (unrollMulticonn $ sys_repetitions system) (sys_multicons system)
        allChainConnections = concat $
            map chainConnections ([ x | x@(Chain {}) <- sys_repetitions system])

        -- (constConns, constCmps) = unrollConstDrivers (sys_connections system) (prg_cmps program)
        


unrollRepetition :: Repetition -> ([Component] -> [Instance])
unrollRepetition rep = case rep of
    Chain {} -> (\cs -> unrollChain cs rep)
    Repeat {} -> (\cs -> unrollRepeat cs rep)

