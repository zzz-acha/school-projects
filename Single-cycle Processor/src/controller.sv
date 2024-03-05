`timescale 1ns / 1ps
module controller(input  logic [5:0] op, funct,
                  input  logic       zero,
                  output logic       memtoreg, memwrite,
                  output logic       pcsrc, alusrc,
                  output logic       regdst, regwrite,
                  output logic       jump,
                  output logic [3:0] alucontrol);

  logic [2:0] aluop;
  logic       branch;
  logic       dst;
  
  maindec md(op, memtoreg, memwrite, branch,
             alusrc, dst, regwrite, jump, aluop);
  aludec  ad(funct, aluop, alucontrol, runxor);

  assign pcsrc = branch & zero;
  assign regdst = dst & ~runxor;
endmodule