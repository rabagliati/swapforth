`timescale 1 ns / 1 ps

`default_nettype none

module forth_wrapper
#(
    parameter LOG2ABITS=11,
    parameter DWIDTH = 16
)
(
);

reg clk, reset;

wire io_rd, io_wr;
wire [DWIDTH-1:0] addra, addrb;
wire wea, web;
wire [DWIDTH-1:0] reada, readb;
wire [DWIDTH-1:0] writea, writeb;

ram_dual
#(
    .MEM_INIT_FILE("/home/andyr/git/vivado/src/swapforth/j1a/digilent/nuc.mcs"),
    .LOG2ABITS(LOG2ABITS),
    .DWIDTH(DWIDTH)
) ram (
    .addra(addra),      .addrb(addrb),
    .reada(reada),      .readb(readb),
    .writea(writea),    .writeb(writeb),
    .wea(wea),          .web(web),
    .clk(clk)
);

j1 _j1(
    .mem_wr(web),
    .dout(writeb),
    .mem_addr(addrb),
    .io_din(readb),
    .code_addr(addra),
    .insn(reada),
    .clk(clk),
    .resetq(reset)
);

always #5 clk = (clk === 1'b0);

initial begin
    @(posedge clk);
    @(posedge clk); reset = 1;
    @(posedge clk); @(posedge clk); @(posedge clk);
    @(posedge clk); reset = 0;
    #1000 $finish;
end

endmodule // top
