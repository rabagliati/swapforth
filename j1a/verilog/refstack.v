module Stack #(
    parameter WIDTH = 11,
    parameter DEPTH = 7
) (
    input                    clk,
    input      [WIDTH - 1:0] d,
    output reg [WIDTH - 1:0] q,
    input                    push,
    input                    pop,
    input                    reset
);

reg [DEPTH - 1:0] ptr;
reg [WIDTH - 1:0] stack [0:(1 << DEPTH) - 1];

always @(posedge clk) begin
    if (reset)
        ptr <= 0;
    else if (push)
        ptr <= ptr + 1;
    else if (pop)
        ptr <= ptr - 1;
end

always @(posedge clk) begin
    if (push || pop) begin
        if(push)
            stack[ptr] <= d;

        q <= stack[ptr - 1];
    end
end

endmodule
// vim:set shiftwidth=4 softtabstop=4 expandtab:
