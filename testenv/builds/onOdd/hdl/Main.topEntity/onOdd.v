/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.3. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module onOdd
    ( // Inputs
      input  clk // clock
    , input  rst // reset
    , input  en // enable
    , input [8:0] value

      // Outputs
    , output wire [8:0] res
    );
  wire [8:0] c$app_arg;
  // testenv/builds/onOdd/Synth_onOdd.hs:5:1-5
  wire [7:0] v;
  wire [8:0] result;

  assign res = result;

  assign c$app_arg = value[8:8] ? {1'b1,((v << (64'sd1)) + v) + 8'd1} : {1'b0,8'bxxxxxxxx};

  assign v = value[7:0];

  assign result = c$app_arg;


endmodule

