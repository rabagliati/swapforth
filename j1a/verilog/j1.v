`default_nettype none
`define WIDTH 16

//Stealing from

//-----------------------------------------------------------------------------
//| @file h2.vhd
//| @brief The H2 Processor: J1 processor translation and extension.
//| Moved bit 12 to bit 4 to allow for more ALU instructions.
//|
//| @author         Richard James Howe.
//| @copyright      Copyright 2017, 2019 Richard James Howe.
//| @license        MIT
//| @email          howe.r.j.89@gmail.com
//|
//| NB. It would be nice to be able to specify the CPU word length with a
//| generic, so we could instantiate a 32-bit CPU if we wanted to.
//-----------------------------------------------------------------------------

module j1 (
    input wire clk,
    input wire reset,

    output wire [15:0] mem_addr,
    output wire mem_wr,
    output wire [`WIDTH-1:0] dout,

    input  wire [`WIDTH-1:0] din,

    output wire [12:0] code_addr,
    input  wire [15:0] insn     // instruction
);

reg [3:0] dsp, dspN;          // data stack pointer
reg [`WIDTH-1:0] st0, st0N;   // top of data stack

reg [12:0] pc /* verilator public_flat */, pcN;           // program counter
wire [12:0] pc_plus_1 = pc + 13'd1;
reg rPush;                    // return stack write
wire [`WIDTH-1:0] rstkD;      // return stack write value
reg reboot = 1;

assign mem_addr = st0[15:0];
assign code_addr = pcN;

// The D and R stacks
wire [`WIDTH-1:0] st1, rst0;
reg [1:0] dspI, rspI;
refstack #(
    .WIDTH(16),
    .DEPTH(15)
) dstack (
    .clk(clk),
    .pop(dspI == 2'b01),
    .out(st1),
    .push(dPush),
    .in(st0),
    .reset(reset)
);

refstack #(
    .WIDTH(16),
    .DEPTH(17)
) rstack (
    .clk(clk),
    .pop(rspI == 3'b01),
    .out(rst0),
    .push(rPush),
    .in(rstkD),
    .reset(reset)
);

//
//	+---------------------------------------------------------------+
//	| F | E | D | C | B | A | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
//	+---------------------------------------------------------------+
//	| 1 |                    LITERAL VALUE                          |
//	+---------------------------------------------------------------+
//	| 0 | 1 | 1 |   ALU OPERATION   |T2N|T2R|N2A|R2P| RSTACK| DSTACK|
//	+---------------------------------------------------------------+
//	| 0 | 1 | 0 |X++|Y++|Z++|X--|Y--|Z--|    EXTENDED OPERATION     |
//	+---------------------------------------------------------------+
//	| 0 | 0 | 1 | 1 |        EXTEND TARGET ADDRESS                  |
//	+---------------------------------------------------------------+
//	| 0 | 0 | 1 | 0 |        CALL TARGET ADDRESS                    |
//	+---------------------------------------------------------------+
//	| 0 | 0 | 0 | 1 |        CONDITIONAL BRANCH TARGET ADDRESS      |
//	+---------------------------------------------------------------+
//	| 0 | 0 | 0 | 0 |        BRANCH TARGET ADDRESS                  |
//	+---------------------------------------------------------------+
//	| F | E | D | C | B | A | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
//	+---------------------------------------------------------------+
//
//	T   : Top of data stack
//	N   : Next on data stack
//	PC  : Program Counter
//
//	LITERAL VALUES : push a value onto the data stack
//	CONDITIONAL    : BRANCHS pop and test the T
//	CALLS          : PC+1 onto the return stack
//
//	T2N : Move T to N
//	T2R : Move T to top of return stack
//	N2A : STORE T to memory location addressed by N
//	R2P : Move top of return stack to PC
//
//	RSTACK and DSTACK are signed values (twos complement) that are
//	the stack delta (the amount to increment or decrement the stack
//	by for their respective stacks: return and data)
//
wire [16:0] minus = {1'b1, ~st0} + st1 + 1;
wire signedless = st0[15] ^ st1[15] ? st1[15] : minus[16];

wire func_T2N = insn[4];
wire func_T2R = insn[5];
wire func_N2A = insn[6];

wire is_lit     = (insn[15]    == 1'b1);
wire is_alu     = (insn[15:13] == 4'b011);
//wire is_special = (insn[15:12] == 4'b0011);
//wire is_call    = (insn[15:12] == 4'b0010);
//wire is_branch0 = (insn[15:12] == 4'b0001);
//wire is_branch  = (insn[15:12] == 4'b0000);
//wire branchaddr = insn[11:0];
wire is_call    = (insn[15:13] == 3'b010);
wire is_branch0 = (insn[15:13] == 3'b001);
wire is_branch  = (insn[15:13] == 3'b000);
wire branchaddr = insn[12:0];

wire alu_op = insn[12:8];
wire is_ram_write = (is_alu & func_N2A);

wire more = ($signed(st0) > $signed(st1));
wire equal = (st0 == st1);
wire umore = ($unsigned(st0) > $unsigned(st1));
wire zero = (st0 == 0);

wire we = (is_ram_write & (st0[15:13] == 3'b000));  // within range
wire dPush = is_lit | (is_alu & func_T2N);          // push data stack

always @* begin                       // Compute the new value of st0
    if (is_lit)
        st0N = {1'b0, insn[14:0]};            // literal
    else if (is_branch | is_call)
        st0N = st0;
    else if (is_branch0)
        st0N = st1;                                // pop condition
    else if (is_alu) begin                            // ALU operations...
        casez (alu_op)
            5'b00000: st0N = st0;                                   // T
            5'b00001: st0N = st1;                                   // N
            5'b00010: st0N = st0 + st1;                             // T+N
            5'b00011: st0N = st0 & st1;                             // T&N
            5'b00100: st0N = st0 | st1;                             // T|N
            5'b00101: st0N = st0 ^ st1;                             // T^N
            5'b00110: st0N = ~st0;

            5'b00111: st0N = {`WIDTH{equal}};                       // N=T
            5'b01000: st0N = {`WIDTH{more}};                        // N<T

            5'b01001: st0N = {st0[`WIDTH - 1], st0[`WIDTH - 1:1]};  // T>>1 (a)
            5'b01010: st0N = st0 - 1;                               // T-1
            5'b01011: st0N = rst0;                                  // R
            5'b01100: st0N = din;                                   // [T]
            5'b01101: st0N = {st0[`WIDTH - 2:0], 1'b0};             // T<<1
            5'b01110: st0N = {{(`WIDTH - 4){1'b0}}, dsp};           // depth
            5'b01111: st0N = {`WIDTH{umore}};                       // u<
            default:  st0N = {`WIDTH{1'bx}};
        endcase
    end
    else
        st0N = {`WIDTH{1'bx}};
end

assign mem_wr = !reboot & is_alu & func_N2A;
assign dout = st1;

assign rstkD = (is_call) ?                      // call
    {{(`WIDTH - 14){1'b0}}, pc_plus_1, 1'b0}    // pc, word boundary
    : st0;                                      // T2R

always @* begin                                 // stacks, pc
    if (is_lit)                                 // data stack
        dspI = 2'b01;
    else if (is_branch0)                        // branch0, pop condition
        dspI = 2'b11;
    else if (is_alu)
        dspI = insn[1:0];                       // out of the instruction
    else if (is_jump | is_call)                 // jump, call
        dspI = 2'b00;                           // nothing
    else                                        // other ALU
        dspI = 2'b00;                           // nothing

    dspN = dsp + {dspI[1], dspI[1], dspI};      // poor man sign extend

    if (is_call)                                // return stack
        rspI = 2'b01;                           // push
    else if (is_alu)
        rspI = insn[3:2];
    else
        rspI = 2'b00;                           // nothing
    rPush = (rspI == 2'b01);

    if (reboot)
        pcN = 0;
    else if (is_branch | is_call | (is_branch0 & (~|st0)))
        pcN = branchaddr;
    else
        pcN = pc_plus_1;
end

always @(posedge clk)
begin
    if (reset) begin
        reboot <= 1'b1;
        { pc, dsp, st0} <= 0;
    end else begin
        reboot <= 0;
        { pc, dsp, st0} <= { pcN, dspN, st0N };
    end
end

endmodule
// vim:set shiftwidth=4 softtabstop=4 expandtab:
