/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.6. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module onEven
    ( // Inputs
      input  clk // clock
    , input  rst // reset
    , input  en // enable
    , input [16:0] val

      // Outputs
    , output wire [16:0] res
    );
  // builds/onEven/Synth_onEven.hs:(11,1)-(12,56)
  wire [16:0] c$ds_app_arg;
  // builds/onEven/Synth_onEven.hs:5:1-6
  wire [15:0] v;

  assign c$ds_app_arg = val[16:16] ? {1'b1,v >> (64'sd1)} : {1'b0,16'bxxxxxxxxxxxxxxxx};

  assign v = val[15:0];

  assign res = c$ds_app_arg;


endmodule
