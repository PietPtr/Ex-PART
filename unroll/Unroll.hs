module Unroll where

import Types
import Chain
import Repeat
import Multiconnection
import Constant -- cancelled, but keeping in the parsing features if time is left over

-- TODO: delete components not found in expi
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
                -- constConns ++ -- TODO: wat een hack om deze eerder te zetten (zie Flattener.hs)
                sys_connections system ++ 
                unrolledConnections ++ 
                allChainConnections
            -- TODO: this is not recursive?! Everything in a nested system is broken now :P
        }
        program' = program {
            prg_cmps = prg_cmps program -- ++ constCmps
        }

        unrolledInstances = concat $ map (\r -> (unrollRepetition r) (prg_cmps program)) (sys_repetitions system)
        unrolledConnections = concat $ 
            map (unrollMulticonn $ sys_repetitions system) (sys_multicons system)
        allChainConnections = concat $
            map chainConnections ([ x | x@(Chain {}) <- sys_repetitions system])

        -- constConns = makeConstantConnections (sys_connections system)
        


unrollRepetition :: Repetition -> ([Component] -> [Instance])
unrollRepetition rep = case rep of
    Chain {} -> (\cs -> unrollChain cs rep)
    Repeat {} -> (\cs -> unrollRepeat cs rep)

