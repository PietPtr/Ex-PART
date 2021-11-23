module Main where

import           Prelude

-- import Compiler
import           System.Environment

import           Elaboration
import           Generator
import           JSONBuilder
import           Parser
import           Yosys
import Nextpnr
import Compiler

-- import stuff such that in GHCi we can inspect types
import           Types

main :: IO ()
main = undefined

-- Untested
-- main :: IO ()
-- main = do
--     args <- getArgs
--     parse args
--     where
--         parse [] = usage
--         parse [_] = usage
--         parse [flowName, dir] = make (flowMap flowName) dir
--         parse _ = usage
--         flowMap name = case name of
--             "resource" -> resource
--             "auto" -> auto
--             "clean" -> clean
--             "monolithic" -> monolithic
--             _ -> error "Main.hs: Flow not found."
--         usage =  putStrLn "Provide a flow name (clean, auto, resource, monolithic) and the name of one of the directories in examples/"
test_design =
  Design
    { des_defs = ["type Bitwidth = Unsigned 8"]
    , des_cmbs = [Combinatory "\n\n"]
    , des_cmps =
        [ Component
            { cmp_name = "compute"
            , cmp_args = []
            , cmp_isoStats =
                [ SInput "inp" "Bitwidth"
                , SState "s" (Constant 0) "Bitwidth"
                , SOutput "out" "Bitwidth"
                ]
            , cmp_where = "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
            }
        ]
    , des_systree =
        SystemTree
          { systr_flattened = False
          , systr_name = "system"
          , systr_size = (26, 26)
          , systr_coords = (CConst 2, CConst 2)
          , systr_iodefs =
              [Output "after_n" "Bitwidth", Input "data_in" "Bitwidth"]
          , systr_instances = []
          , systr_connections =
              [ Connection (CID "compute_blocks_8" "out") (CID "this" "after_n")
              , Connection (CID "this" "data_in") (CID "compute_blocks_1" "inp")
              ]
          , systr_repetitions =
              [ RawChain
                  "compute_blocks"
                  (CConst 0, CConst 0)
                  [ Comp (UnplacedInstance "compute" [] (7, 4))
                  , Amount 8
                  , Layout "horizontal"
                  , ChainIn "inp"
                  , ChainOut "out"
                  ]
              ]
          , systr_multicons = []
          , systr_subsystems = []
          , systr_constantDrivers = []
          }
    }

test_system =
  System
    { sys_name = "system"
    , sys_topdata =
        TopData
          { top_defs = ["type Bitwidth = Unsigned 8"]
          , top_cmbs = [Combinatory "\n\n"]
          , top_cmps =
              [ Component
                  { cmp_name = "compute"
                  , cmp_args = []
                  , cmp_isoStats =
                      [ SInput "inp" "Bitwidth"
                      , SState "s" (Constant 0) "Bitwidth"
                      , SOutput "out" "Bitwidth"
                      ]
                  , cmp_where = "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                  }
              ]
          }
    , sys_size = (26, 26)
    , sys_coords = (CConst 2, CConst 2)
    , sys_iodefs = [Output "after_n" "Bitwidth", Input "data_in" "Bitwidth"]
    , sys_elems =
        [ Element
            { elem_name = "compute_blocks_1"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords = (CConst 0, CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_1"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords = (CConst 0, CConst 0)
                     })
            }
        , Element
            { elem_name = "compute_blocks_2"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords =
                ( CAdd (CX "compute_blocks_1") (CWidth "compute_blocks_1")
                , CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_2"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords =
                         ( CAdd
                             (CX "compute_blocks_1")
                             (CWidth "compute_blocks_1")
                         , CConst 0)
                     })
            }
        , Element
            { elem_name = "compute_blocks_3"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords =
                ( CAdd (CX "compute_blocks_2") (CWidth "compute_blocks_2")
                , CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_3"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords =
                         ( CAdd
                             (CX "compute_blocks_2")
                             (CWidth "compute_blocks_2")
                         , CConst 0)
                     })
            }
        , Element
            { elem_name = "compute_blocks_4"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords =
                ( CAdd (CX "compute_blocks_3") (CWidth "compute_blocks_3")
                , CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_4"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords =
                         ( CAdd
                             (CX "compute_blocks_3")
                             (CWidth "compute_blocks_3")
                         , CConst 0)
                     })
            }
        , Element
            { elem_name = "compute_blocks_5"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords =
                ( CAdd (CX "compute_blocks_4") (CWidth "compute_blocks_4")
                , CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_5"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords =
                         ( CAdd
                             (CX "compute_blocks_4")
                             (CWidth "compute_blocks_4")
                         , CConst 0)
                     })
            }
        , Element
            { elem_name = "compute_blocks_6"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords =
                ( CAdd (CX "compute_blocks_5") (CWidth "compute_blocks_5")
                , CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_6"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords =
                         ( CAdd
                             (CX "compute_blocks_5")
                             (CWidth "compute_blocks_5")
                         , CConst 0)
                     })
            }
        , Element
            { elem_name = "compute_blocks_7"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords =
                ( CAdd (CX "compute_blocks_6") (CWidth "compute_blocks_6")
                , CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_7"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords =
                         ( CAdd
                             (CX "compute_blocks_6")
                             (CWidth "compute_blocks_6")
                         , CConst 0)
                     })
            }
        , Element
            { elem_name = "compute_blocks_8"
            , elem_type = "compute"
            , elem_size = (7, 4)
            , elem_coords =
                ( CAdd (CX "compute_blocks_7") (CWidth "compute_blocks_7")
                , CConst 0)
            , elem_iodefs = [Input "inp" "Bitwidth", Output "out" "Bitwidth"]
            , elem_implementation =
                InstanceImpl
                  (Instance
                     { ins_name = "compute_blocks_8"
                     , ins_cmp =
                         Component
                           { cmp_name = "compute"
                           , cmp_args = []
                           , cmp_isoStats =
                               [ SInput "inp" "Bitwidth"
                               , SState "s" (Constant 0) "Bitwidth"
                               , SOutput "out" "Bitwidth"
                               ]
                           , cmp_where =
                               "s' = inp\n\n    out = (s * 13) `mod` 17\n\n"
                           }
                     , ins_args = []
                     , ins_size = (7, 4)
                     , ins_coords =
                         ( CAdd
                             (CX "compute_blocks_7")
                             (CWidth "compute_blocks_7")
                         , CConst 0)
                     })
            }
        ]
    , sys_connections =
        [ Connection' (CID "compute_blocks_8" "out") (CID "this" "after_n") 16
        , Connection' (CID "this" "data_in") (CID "compute_blocks_1" "inp") 16
        , Connection'
            (CID "compute_blocks_1" "out")
            (CID "compute_blocks_2" "inp")
            16
        , Connection'
            (CID "compute_blocks_2" "out")
            (CID "compute_blocks_3" "inp")
            16
        , Connection'
            (CID "compute_blocks_3" "out")
            (CID "compute_blocks_4" "inp")
            16
        , Connection'
            (CID "compute_blocks_4" "out")
            (CID "compute_blocks_5" "inp")
            16
        , Connection'
            (CID "compute_blocks_5" "out")
            (CID "compute_blocks_6" "inp")
            16
        , Connection'
            (CID "compute_blocks_6" "out")
            (CID "compute_blocks_7" "inp")
            16
        , Connection'
            (CID "compute_blocks_7" "out")
            (CID "compute_blocks_8" "inp")
            16
        ]
    , sys_constantDrivers = []
    }
