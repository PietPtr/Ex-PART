/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.6. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module system
    ( // Inputs
      input  clk // clock
    , input  rst // reset
    , input  en // enable
    , input [16:0] setting

      // Outputs
    , output wire [15:0] result
    );
  // Clash.hs:(49,1)-(50,57)
  wire [15:0] c$ds_app_arg;
  // Clash.hs:12:1-7
  reg [15:0] last_val = 16'd0;
  // Clash.hs:12:1-7
  wire [15:0] new_value;
  wire [33:0] result_1;
  wire [33:0] result_2;
  wire [16:0] c$app_arg;
  wire [16:0] c$app_arg_0;
  wire  c$case_scrut;
  wire [16:0] result_3;
  wire [16:0] c$app_arg_1;
  // Clash.hs:30:1-5
  wire [15:0] v;
  wire [16:0] result_4;
  // Clash.hs:(61,1)-(62,56)
  wire [16:0] c$ds_app_arg_2;
  // Clash.hs:37:1-6
  wire [15:0] v_0;
  // Clash.hs:(53,1)-(54,63)
  wire [15:0] c$ds_case_alt;
  // Clash.hs:(53,1)-(54,63)
  wire [15:0] c$ds_app_arg_3;
  // Clash.hs:21:1-6
  wire [15:0] v_1;
  // Clash.hs:21:1-6
  wire [15:0] v_2;
  wire [15:0] c$bv;
  wire [16:0] c$app_arg_selection_3;
  wire [16:0] c$ds_app_arg_selection_2;

  assign c$ds_app_arg = setting[16:16] ? new_value : c$ds_app_arg_3;

  // register begin
  always @(posedge clk or  posedge  rst) begin : last_val_register
    if ( rst) begin
      last_val <= 16'd0;
    end else if (en) begin
      last_val <= c$ds_app_arg;
    end
  end
  // register end

  assign new_value = setting[15:0];

  assign result_1 = result_2;

  assign result_2 = {c$app_arg_0,   c$app_arg};

  assign c$app_arg = c$case_scrut ? {1'b0,16'bxxxxxxxxxxxxxxxx} : {1'b1,last_val};

  assign c$app_arg_0 = c$case_scrut ? {1'b1,last_val} : {1'b0,16'bxxxxxxxxxxxxxxxx};

  assign c$bv = (last_val);

  assign c$case_scrut = (c$bv[(64'sd0)]) == (1'b1);

  assign result_3 = result_4;

  assign c$app_arg_selection_3 = result_1[33:17];

  assign c$app_arg_1 = c$app_arg_selection_3[16:16] ? {1'b1,((v << (64'sd1)) + v) + 16'd1} : {1'b0,16'bxxxxxxxxxxxxxxxx};

  assign v = result_1[32:17];

  assign result_4 = c$app_arg_1;

  assign c$ds_app_arg_selection_2 = result_1[16:0];

  assign c$ds_app_arg_2 = c$ds_app_arg_selection_2[16:16] ? {1'b1,v_0 >> (64'sd1)} : {1'b0,16'bxxxxxxxxxxxxxxxx};

  assign v_0 = result_1[15:0];

  assign c$ds_case_alt = c$ds_app_arg_2[16:16] ? v_2 : 16'd0;

  assign c$ds_app_arg_3 = result_3[16:16] ? v_1 : c$ds_case_alt;

  assign v_1 = result_3[15:0];

  assign v_2 = c$ds_app_arg_2[15:0];

  assign result = last_val;


endmodule

