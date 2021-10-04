module Nextpnr where


-- TODO: Think about how exactly to integrate nextpnr, delivering just the synthesized and locations json could be enough perhaps...
-- nextpnr :: IO ()
-- nextpnr = do
--     process <- pure $ proc "nextpnr-ecp5"
--         ["--85k",
--          "--json",
--          "combined.json",
--          ""]

--     	nextpnr-ecp5 --85k --json ../testenv/combined.json \
-- 		--lpf collatz.lpf \
-- 		--textcfg collatzer.config \
-- 		--pre-place constrainer.py 