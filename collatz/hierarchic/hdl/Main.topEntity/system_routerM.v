/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.6. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module system_routerM
    ( // Inputs
      input  c$ds_bindCsr // clock
    , input  c$ds_bindCsr_0 // reset
    , input  c$ds_bindCsr_1 // enable
    , input [15:0] eta

      // Outputs
    , output wire [33:0] result
    );
  wire [33:0] result_0;
  wire [16:0] c$app_arg;
  wire [16:0] c$app_arg_0;
  wire  c$case_scrut;
  wire [15:0] c$bv;

  assign result = result_0;

  assign result_0 = {c$app_arg_0,   c$app_arg};

  assign c$app_arg = c$case_scrut ? {1'b0,16'bxxxxxxxxxxxxxxxx} : {1'b1,eta};

  assign c$app_arg_0 = c$case_scrut ? {1'b1,eta} : {1'b0,16'bxxxxxxxxxxxxxxxx};

  assign c$bv = (eta);

  assign c$case_scrut = (c$bv[(64'sd0)]) == (1'b1);


endmodule

