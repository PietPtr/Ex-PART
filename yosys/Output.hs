test =
  [ Module
      { mod_name = "system"
      , mod_top = True
      , mod_ports =
          [ Port
              { port_name = "result"
              , port_direction = Out
              , port_bits = [5, 6, 7, 8, 9, 10, 11, 12]
              }
          , Port
              { port_name = "setting"
              , port_direction = In
              , port_bits = [13, 14, 15, 16, 17, 18, 19, 20, 21]
              }
          ]
      , mod_cells =
          [ Cell
              { cell_name = "collatzer"
              , cell_type = "collatzer"
              , cell_connections =
                  [ CellConn "clk" [2]
                  , CellConn "rst" [3]
                  , CellConn "en" [4]
                  , CellConn "val_out" [39, 40, 41, 42, 43, 44, 45, 46]
                  , CellConn "val_in" [5, 6, 7, 8, 9, 10, 11, 12]
                  ]
              }
          , Cell
              { cell_name = "controller"
              , cell_type = "control"
              , cell_connections =
                  [ CellConn "clk" [2]
                  , CellConn "rst" [3]
                  , CellConn "en" [4]
                  , CellConn "next_val" [39, 40, 41, 42, 43, 44, 45, 46]
                  , CellConn "set_val" [13, 14, 15, 16, 17, 18, 19, 20, 21]
                  , CellConn "result_value" [5, 6, 7, 8, 9, 10, 11, 12]
                  ]
              }
          ]
      }
  , Module
      { mod_name = "collatzer"
      , mod_top = False
      , mod_ports =
          [ Port
              { port_name = "val_out"
              , port_direction = Out
              , port_bits = [5, 6, 7, 8, 9, 10, 11, 12]
              }
          , Port
              { port_name = "val_in"
              , port_direction = In
              , port_bits = [13, 14, 15, 16, 17, 18, 19, 20]
              }
          ]
      , mod_cells =
          [ Cell
              { cell_name = "merger"
              , cell_type = "merger"
              , cell_connections =
                  [ CellConn "clk" [2]
                  , CellConn "rst" [3]
                  , CellConn "en" [4]
                  , CellConn "vo" [38, 39, 40, 41, 42, 43, 44, 45, 46]
                  , CellConn "ve" [29, 30, 31, 32, 33, 34, 35, 36, 37]
                  , CellConn "res" [5, 6, 7, 8, 9, 10, 11, 12]
                  ]
              }
          , Cell
              { cell_name = "onEven"
              , cell_type = "onEven"
              , cell_connections =
                  [ CellConn "clk" [2]
                  , CellConn "rst" [3]
                  , CellConn "en" [4]
                  , CellConn "val" [47, 48, 49, 50, 51, 52, 53, 54, 55]
                  , CellConn "res" [29, 30, 31, 32, 33, 34, 35, 36, 37]
                  ]
              }
          , Cell
              { cell_name = "onOdd"
              , cell_type = "onOdd"
              , cell_connections =
                  [ CellConn "clk" [2]
                  , CellConn "rst" [3]
                  , CellConn "en" [4]
                  , CellConn "val" [56, 57, 58, 59, 60, 61, 62, 63, 64]
                  , CellConn "res" [38, 39, 40, 41, 42, 43, 44, 45, 46]
                  ]
              }
          , Cell
              { cell_name = "router"
              , cell_type = "router"
              , cell_connections =
                  [ CellConn "clk" [2]
                  , CellConn "rst" [3]
                  , CellConn "en" [4]
                  , CellConn "val" [13, 14, 15, 16, 17, 18, 19, 20]
                  , CellConn "odd" [56, 57, 58, 59, 60, 61, 62, 63, 64]
                  , CellConn "even" [47, 48, 49, 50, 51, 52, 53, 54, 55]
                  ]
              }
          ]
      }
  ]
