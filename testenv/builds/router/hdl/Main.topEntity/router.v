/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.3. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module router
    ( // Inputs
      input  clk // clock
    , input  rst // reset
    , input  en // enable
    , input [7:0] val

      // Outputs
    , output wire [8:0] odd
    , output wire [8:0] even
    );
  wire [17:0] result_0;
  wire [8:0] c$app_arg;
  wire [8:0] c$app_arg_0;
  wire  c$case_scrut;
  wire [7:0] c$bv;
  wire [17:0] result;

  assign result = result_0;

  assign result_0 = {c$app_arg_0,   c$app_arg};

  assign c$app_arg = c$case_scrut ? {1'b1,val} : {1'b0,8'bxxxxxxxx};

  assign c$app_arg_0 = c$case_scrut ? {1'b0,8'bxxxxxxxx} : {1'b1,val};

  assign c$bv = (val);

  assign c$case_scrut = (c$bv[(64'sd0)]) == (1'b1);

  assign odd = result[17:9];

  assign even = result[8:0];


endmodule

