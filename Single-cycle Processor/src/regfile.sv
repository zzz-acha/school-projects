`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 12/09/2022 04:13:55 PM
// Design Name: 
// Module Name: regfile
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module regfile(input  logic        clk, 
               input  logic        we3, 
               input  logic [4:0]  ra1, ra2, wa3, 
               input  logic [31:0] wd3, 
               output logic [31:0] rd1, rd2);

  logic [31:0] rf[31:0]; //creates a 32-value array rf with 32 bits each

  // three ported register file
  // read two ports combinationally
  // write third port on rising edge of clk
  // register 0 hardwired to 0
  // note: for pipelined processor, write on
  // falling edge of clk

  always_ff @(posedge clk)
    if (we3) rf[wa3] <= wd3; //if we3 is 1, set the wa3rd rf to be wd3

  assign rd1 = (ra1 != 0) ? rf[ra1] : 0; //at any given time, if ra1 is not equal to 0, the output rd1 rf[ra1]; otherwise, it is 0
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0; //at any given time, if ra1 is not equal to 0, the output rd1 rf[ra1]; otherwise, it is 0
endmodule
