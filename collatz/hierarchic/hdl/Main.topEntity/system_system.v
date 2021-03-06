/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.6. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module system_system
    ( // Inputs
      input  c$bindCsr // clock
    , input  c$bindCsr_0 // reset
    , input  c$bindCsr_1 // enable
    , input [16:0] eta

      // Outputs
    , output wire [15:0] result
    );
  wire [33:0] result_0;
  wire [16:0] result_1;
  wire [16:0] result_2;
  wire [15:0] result_3;
  wire [15:0] c$result_rec;
  wire [16:0] result_fun_arg;
  wire [16:0] result_fun_arg_0;
  wire [33:0] result_fun_arg_1;
  wire [32:0] c$result_rec_fun_arg;

  system_routerM system_routerM_result_0
    ( .result (result_0)
    , .c$ds_bindCsr (c$bindCsr)
    , .c$ds_bindCsr_0 (c$bindCsr_0)
    , .c$ds_bindCsr_1 (c$bindCsr_1)
    , .eta (c$result_rec) );

  assign result_fun_arg = result_0[33:17];

  system_onOddM system_onOddM_result_1
    ( .result (result_1)
    , .c$controller_result_value_bindCsr (c$bindCsr)
    , .c$controller_result_value_bindCsr_0 (c$bindCsr_0)
    , .c$controller_result_value_bindCsr_1 (c$bindCsr_1)
    , .eta (result_fun_arg) );

  assign result_fun_arg_0 = result_0[16:0];

  system_onEvenM system_onEvenM_result_2
    ( .result (result_2)
    , .c$controller_result_value_bindCsr (c$bindCsr)
    , .c$controller_result_value_bindCsr_0 (c$bindCsr_0)
    , .c$controller_result_value_bindCsr_1 (c$bindCsr_1)
    , .eta (result_fun_arg_0) );

  assign result_fun_arg_1 = {result_1,
                             result_2};

  system_mergerM system_mergerM_result_3
    ( .result (result_3)
    , .c$controller_result_value_bindCsr (c$bindCsr)
    , .c$controller_result_value_bindCsr_0 (c$bindCsr_0)
    , .c$controller_result_value_bindCsr_1 (c$bindCsr_1)
    , .eta (result_fun_arg_1) );

  assign c$result_rec_fun_arg = {result_3,   eta};

  system_controlM system_controlM_c$result_rec
    ( .result (c$result_rec)
    , .c$controller_result_value_bindCsr (c$bindCsr)
    , .c$controller_result_value_bindCsr_0 (c$bindCsr_0)
    , .c$controller_result_value_bindCsr_1 (c$bindCsr_1)
    , .i1 (c$result_rec_fun_arg) );

  assign result = c$result_rec;


endmodule

