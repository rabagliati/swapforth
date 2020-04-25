`timescale 1 ns / 1 ps

module refstack #(
    parameter WIDTH = 16,
    parameter DEPTHLOG2 = 3
) (
    input wire               clk,
    input wire [WIDTH - 1:0] in,
    output reg [WIDTH - 1:0] out,
    input wire               push,
    input wire               pop,
    input wire               reset
);

reg [DEPTHLOG2 - 1:0] ptr;
reg [WIDTH - 1:0] stack [0:(1<<DEPTHLOG2)-1];

always @(posedge clk) begin
    if      (reset)     ptr <= 0;
    if (pop & push)    // pop current, push new
    begin
        out <= in;
        stack[ptr-1] <= in;
    end
    else if (push) begin
        out <= in;
        stack[ptr] <= in;
        ptr <= ptr + 1;
    end
    else if (pop) begin
        out <= stack[ptr - 2];
        ptr <= ptr - 1;
    end
end

endmodule
// vim:set shiftwidth=4 softtabstop=4 expandtab:
