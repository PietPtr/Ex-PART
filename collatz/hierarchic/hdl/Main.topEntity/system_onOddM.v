/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.6. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module system_onOddM
    ( // Inputs
      input  c$controller_result_value_bindCsr // clock
    , input  c$controller_result_value_bindCsr_0 // reset
    , input  c$controller_result_value_bindCsr_1 // enable
    , input [16:0] eta

      // Outputs
    , output wire [16:0] result
    );
  wire [16:0] c$app_arg;
  // Clash.hs:39:1-5
  wire [15:0] v;
  wire [16:0] result_0;

  assign result = result_0;

  assign c$app_arg = eta[16:16] ? {1'b1,((v << (64'sd1)) + v) + 16'd1} : {1'b0,16'bxxxxxxxxxxxxxxxx};

  assign v = eta[15:0];

  assign result_0 = c$app_arg;


endmodule

