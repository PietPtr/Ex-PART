{-# LANGUAGE RecordWildCards #-}

module Chain where

import Types
import Repeat -- chain is a specialization of repeat

-- todo: swap arguments here instead of in lambda?
unrollChain :: [Component] -> Repetition -> [Instance]
unrollChain cmps chain = map (makeInstance cmps chain) [0..(chn_amount chain - 1)]

chainConnections :: Repetition -> [Connection]
chainConnections chain = map makeConnection [0..(chn_amount - 2)]
    where
        Chain {..} = chain

        makeConnection i = Connection from to
            where
                from = CID (chn_name ++ "_" ++ show i) chn_chainOut
                to = CID (chn_name ++ "_" ++ show (i + 1)) chn_chainIn
                

