module ram_dual
#(
    parameter MEM_INIT_FILE = "",
    parameter LOG2ABITS = 12,   // Ultraram 4kx16
    parameter DWIDTH = 16
)
(
    output reg [DWIDTH-1:0] reada, readb,
    input wire [DWIDTH-1:0] writea, writeb,
    input wire [LOG2ABITS-1:0] addra, addrb,
    input wire wea, web, clk
);

(* ram_init_file = "nuc.mcs" *) reg [DWIDTH-1:0] mem [0:(1<<LOG2ABITS)-1];

initial begin
    if (MEM_INIT_FILE != "") begin
        $readmemh(MEM_INIT_FILE, ram);
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

endmodule
