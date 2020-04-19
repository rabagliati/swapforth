
module ram_dual(q, addr_in, addr_out, d, we, clk1, clk2);
   output[7:0] q;
   input [7:0] d;
   input [6:0] addr_in;
   input [6:0] addr_out;
   input we, clk1, clk2;
 
   reg [6:0] addr_out_reg;
   reg [7:0] q;
   reg [7:0] mem [127:0];
 
   always @(posedge clk1) begin
      if (we)
         mem[addr_in] <= d;
   end
 
   always @(posedge clk2) begin
      q <= mem[addr_out_reg];
      addr_out_reg <= addr_out;
   end
        
endmodule
