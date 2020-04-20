`timescale 1 ns / 1 ps

`default_nettype none

module forth_wrapper
#(
    parameter MEM_INIT_FILE = "/home/andyr/git/vivado/src/swapforth/j1a/digilent/nuc.mcs",
    parameter LOG2ABITS=11,
    parameter DWIDTH = 16
)
(
);

reg clk, reset;

wire io_rd, io_wr;
wire [DWIDTH-1:0] addra, addrb;
wire wea, web;
reg [DWIDTH-1:0] reada, readb;
wire [DWIDTH-1:0] writea, writeb;

(* ram_init_file = "nuc.mcs" *) reg [DWIDTH-1:0] mem [0:(1<<LOG2ABITS)-1];

initial begin
    if (MEM_INIT_FILE != "") begin
        $readmemh(MEM_INIT_FILE, mem);
    end
end

always @(posedge clk) begin
    if (wea)
        mem[addra] <= writea;
    else
        reada <= mem[addra];
end
always @(posedge clk) begin
    if (web)
        mem[addrb] <= writeb;
    else
        readb <= mem[addrb];
end

always #5 clk = (clk === 1'b0);

initial begin
    @(posedge clk);
    @(posedge clk); reset = 1;
    @(posedge clk); @(posedge clk); @(posedge clk);
    @(posedge clk); reset = 0;
    #1000 $finish;
end

endmodule // top
