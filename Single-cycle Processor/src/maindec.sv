`timescale 1ns / 1ps
module maindec(input  logic [5:0] op,
               output logic       memtoreg, memwrite,
               output logic       branch, alusrc,
               output logic       regdst, regwrite,
               output logic       jump,
               output logic [2:0] aluop);

  logic [9:0] controls;

  assign {regwrite, regdst, alusrc, branch, memwrite,
          memtoreg, jump, aluop} = controls;

  always_comb
    case(op)
      6'b000000: controls <= 10'b1100000010; // RTYPE
      6'b100011: controls <= 10'b1010010000; // LW
      6'b101011: controls <= 10'b0010100000; // SW
      6'b000100: controls <= 10'b0001000001; // BEQ
      6'b011101: controls <= 10'b0001000101; // BGTZ
      6'b001000: controls <= 10'b1010000000; // ADDI
      6'b001110: controls <= 10'b1010000011; // XORI
      6'b001111: controls <= 10'b1010000100; // LUI
      6'b010001: controls <= 10'b1010000110; // LI
      6'b000010: controls <= 10'b0000001000; // J
      default:   controls <= 10'bxxxxxxxxxx; // illegal op
    endcase
endmodule