/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.3. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module control
    ( // Inputs
      input  clk // clock
    , input  rst // reset
    , input  en // enable
    , input [15:0] next_val
    , input [16:0] set_val

      // Outputs
    , output wire [15:0] result_value
    );
  // testenv/builds/control/Synth_control.hs:(13,1)-(14,57)
  wire [15:0] c$ds_app_arg;
  // testenv/builds/control/Synth_control.hs:5:1-7
  reg [15:0] last_val = 16'd0;
  // testenv/builds/control/Synth_control.hs:5:1-7
  wire [15:0] next_val_0;
  // testenv/builds/control/Synth_control.hs:5:1-7
  wire [16:0] set_val_0;
  // testenv/builds/control/Synth_control.hs:5:1-7
  wire [15:0] new_value;
  wire [32:0] c$arg;

  assign c$arg = {next_val,   set_val};

  assign c$ds_app_arg = set_val_0[16:16] ? new_value : next_val_0;

  // register begin
  always @(posedge clk or  posedge  rst) begin : last_val_register
    if ( rst) begin
      last_val <= 16'd0;
    end else if (en) begin
      last_val <= c$ds_app_arg;
    end
  end
  // register end

  assign next_val_0 = c$arg[32:17];

  assign set_val_0 = c$arg[16:0];

  assign new_value = set_val_0[15:0];

  assign result_value = last_val;


endmodule

