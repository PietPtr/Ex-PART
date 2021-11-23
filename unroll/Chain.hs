{-# LANGUAGE RecordWildCards #-}

module Chain where

import Types
import Repeat -- chain is a specialization of repeat

unrollChain :: [Component] -> Repetition -> [Instance]
unrollChain cmps chain = map (makeInstance cmps chain) [1..(chn_amount chain)]

chainConnections :: Repetition -> [Connection]
chainConnections chain = map makeConnection [1..(chn_amount - 1)]
    where
        Chain {..} = chain

        makeConnection i = Connection from to
            where
                from = CID (chn_name ++ "_" ++ show i) chn_chainOut
                to = CID (chn_name ++ "_" ++ show (i + 1)) chn_chainIn
                

