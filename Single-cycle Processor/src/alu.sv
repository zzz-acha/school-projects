module alu(input  logic [31:0] a, b,
           input  logic [3:0]  alucontrol,
           output logic [31:0] result,
           output logic        zero);
  
  logic [31:0] condinvb, sum;

  assign condinvb = alucontrol[2] ? ~b : b;
  assign sum = a + condinvb + alucontrol[2];
 
  always_comb
    case(alucontrol)
      4'b0011: result = a ^ b;
      4'b0100: result = b << 16;
      4'b0101: result = b >> a;
      4'b1000: result = (a > 0) ? 0 : 1;
      4'b1001: result = {16'b0, b[15:0]};
      4'b1010: begin
                result[31] = a[31];
                for (int i = 30; i >=0; i--) begin
                  result[i] = a[i] ^ a[i+1];
                end
               end
      default: case (alucontrol[1:0])
          2'b00: result = a & b;
          2'b01: result = a | b;
          2'b10: result = sum;
          2'b11: result = sum[31];
        endcase
     endcase

  assign zero = (result == 32'b0);
endmodule