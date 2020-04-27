`timescale 1 ns / 1 ps

`default_nettype none

module forth_wrapper
#(
    parameter MEM_INIT_FILE = "/home/andyr/git/vivado/src/swapforth/j1a/build/nuc.hex",
    parameter MEMWIDTH =13, // 16k words == 32k bytes
    parameter DWIDTH = 16
)
(
);

reg clk=0, reset=0;

wire [MEMWIDTH-1:0] addra, addrb;
wire wea, web;
reg [DWIDTH-1:0] reada, readb;
wire [DWIDTH-1:0] writea, writeb;

j1 _j1(
    .clk(clk),
    .reset(reset),
    .mem_wr(web),
    .dout(writeb),
    .din(readb),
    .mem_addr(addrb),
    .code_addr(addra),
    .insn(reada));

(* ram_init_file = "nuc.mcs" *) reg [DWIDTH-1:0] mem [0:(1<<MEMWIDTH)-1];

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
    reset = 1;
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    reset = 0;
    #250 $finish;
end

endmodule // top
// vim:set shiftwidth=4 softtabstop=4 expandtab:
