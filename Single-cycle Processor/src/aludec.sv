////////////
`timescale 1ns / 1ps
module aludec(input  logic [5:0] funct,
              input  logic [2:0] aluop,
              output logic [3:0] alucontrol,
              output logic runxor);

  always_comb begin
    runxor <= 0;
    case(aluop)
      3'b000: alucontrol <= 4'b0010;  // add (for lw/sw/addi)
      3'b001: alucontrol <= 4'b0110;  // sub (for beq)\
      3'b011: alucontrol <= 4'b0011;  // xori
      3'b100: alucontrol <= 4'b0100;  // lui
      3'b101: alucontrol <= 4'b1000;  // bgtz
      3'b110: alucontrol <= 4'b1001;  // li
      default: case(funct)          // R-type instructions
          6'b100000: alucontrol <= 4'b0010; // add
          6'b100010: alucontrol <= 4'b0110; // sub
          6'b100100: alucontrol <= 4'b0000; // and
          6'b100101: alucontrol <= 4'b0001; // or
          6'b101010: alucontrol <= 4'b0111; // slt
          6'b000110: alucontrol <= 4'b0101; // srlv
          6'b101101: begin                  // runxor
                        alucontrol <= 4'b1010; 
                        runxor <= 1;
                     end
          default:   alucontrol <= 4'bxxxx; // ???
        endcase
    endcase
    end
endmodule
