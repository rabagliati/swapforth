`default_nettype none
`define WIDTH 16
`define BRANCHWIDTH 11

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

module
#(
    parameter DSTACKLOG2 = 4,
    parameter RSTACKLOG2 = 4) j1 (
    input wire clk,

    output wire [15:0] mem_addr,
    output wire mem_wr,
    output wire [`WIDTH-1:0] dout,

    input  wire [`WIDTH-1:0] din,

    output wire [12:0] code_addr,
    input  wire [15:0] insn,            // instruction
    input wire reset
);

//  the kings of the road
reg [`WIDTH-1:0] st0, st0N;             // top of data stack
reg [12:0] pc /* verilator public_flat */, pcN;           // program counter

// The D and R stack controls
reg dpop, dpush, rpop, rpush;

reg reboot = 1;
reg [`DSTACKLOG2:0] depth, rdepth;      // one bit longer, DEBUG


//
//	+---------------------------------------------------------------+
//	| F | E | D | C | B | A | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
//	+---------------------------------------------------------------+
//	| 1 |                    LITERAL VALUE                          |
//	+---------------------------------------------------------------+
//	| 0 | 1 |CLR|SET|SCH|ADD| XYorZ | F3+MSK| F2+MSK| F1+MSK|DAT+MSK|
//	+---------------------------------------------------------------+
//	| 0 | 0 | 1 | 1 | ALU OPERATION |T2N|T2R|N2A|R2P|PPr|PSH|PPd|PSH|
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
//	N2A : STORE N to memory location addressed by T
//	R2P : Move top of return stack to PC
//
//	RSTACK and DSTACK are signed values (twos complement) that are
//	the stack delta (the amount to increment or decrement the stack
//	by for their respective stacks: return and data)
//
//      T2R cannot be combined with push(return) because the return pop
//      can be used by the optimiser? - andyr
//      similar questions around the push(data)
//

// closely track the Kings above
wire [`WIDTH-1:0] st1, rst0;
wire [12:0] pc_plus_1 = pc + 13'd1;

wire [`WIDTH-1:0] rstkD;      // return stack write value

wire [`WIDTH:0] minus = {1'b1, ~st0} + st1 + 1;     // extra msb bit

wire signedless = st0[15] ^ st1[15] ? st1[15] : minus[16];

wire is_lit     = (insn[15]    == 1'b1);
wire is_special = (insn[15:14] == 2'b01);
wire is_alu     = (insn[15:12] == 4'b0011);
wire is_call    = (insn[15:12] == 4'b0010);
wire is_branch0 = (insn[15:12] == 4'b0001);
wire is_branch  = (insn[15:12] == 4'b0000);
wire [`BRANCHWIDTH:0] branchaddr = insn[`BRANCHWIDTH:0];

wire [4:0] alu_op = insn[`BRANCHWIDTH:7];
wire func_N2A = insn[6];
wire func_T2R = insn[5];
wire func_T2N = insn[4];


wire is_ram_write   = (is_alu & func_N2A);
wire more           = ($signed(st0) > $signed(st1));
wire equal          = (st0 == st1);
wire umore          = ($unsigned(st0) > $unsigned(st1));
wire zero           = (st0 == 0);

// wire we = (is_ram_write & (st0[15:12] == 4'b0000)); // within range

// module outputs, after that introduction
//
assign mem_addr = st0;          // read and write memory always addressed by T
assign code_addr = pcN;         // next instruction fetch

assign mem_wr = !reboot & is_ram_write;
assign dout = st1;                              // we always write from N

// if call is not pushing an address, we write from T, if at all
assign rstkD = (is_call) ?                      // call
    {{(`WIDTH - `BRANCHWIDTH - 2){1'b0}}, pc_plus_1, 1'b0}    // pc, word boundary
    : st0;                                      // T2R

// The D and R stacks
refstack #(
    .WIDTH(16),
    .DEPTHLOG2(DSTACKLOG2)
) dstack (
    .clk(clk),
    .pop(dpop),
    .out(st1),
    .push(dpush),
    .in(st0),
    .reset(reset)
);

refstack #(
    .WIDTH(16),
    .DEPTHLOG2(RSTACKLOG2)
) rstack (
    .clk(clk),
    .pop(rpop),
    .out(rst0),
    .push(rpush),
    .in(rstkD),
    .reset(reset)
);

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
            5'b00110: st0N = ~st0;                                  // ~T

            5'b00111: st0N = {`WIDTH{equal}};                       // N=T
            5'b01000: st0N = {`WIDTH{more}};                        // N<T

            5'b01001: st0N = {st0[`WIDTH - 1], st0[`WIDTH - 1:1]};  // T>>1 (a)
            5'b01010: st0N = st0 - 1;                               // T-1
            5'b01011: st0N = rst0;                                  // R
            5'b01100: st0N = din;                                   // [T]
            5'b01101: st0N = {st0[`WIDTH - 2:0], 1'b0};             // T<<1
            5'b01110: st0N = {3'b0, depth, 3'b0, rdepth};         // debug
            5'b01111: st0N = {`WIDTH{umore}};                       // u<
            default:  st0N = {`WIDTH{1'bx}};
        endcase
    end
    else
        st0N = {`WIDTH{1'bx}};
end

always @* begin                                 // stacks, pc

    // default should be no push or pop - that keeps compiler happy
    // ---------------------------------------
    //                      data stack
    if (is_lit)                                 
        dpush = 1;
    else if (is_branch0)                        // branch0, pop condition
        dpop = 1;
    if (is_alu)
        {dpop, dpush} = insn[1:0];              // out of the instruction
    else if (is_branch | is_call)               // jump, call
        {dpop, dpush} = 2'b00;
    else if (is_special)
        dspI = 2'b00;                           // nothing
    else
        dspI = 2'b00;

    // ---------------------------------------
    //                      return stack
    //                  if no pop, I think you can peephole optimise by adding
    //
    if (is_call)
        rpush = 1;                              // push return addr
    if (is_alu)
        {rpop, rpush} = insn[3:2];              // out of instruction
    else
        rspI = 2'b00;                           // nothing
    rPush = (rspI == 2'b01);

    // ---------------------------------------
    //                      program counter
    //             could put interrupt check here ?
    if (reboot)
        pcN = 0;
    else if (is_branch | is_call | (is_branch0 & (~|st0)))
        pcN = branchaddr;
    else
        pcN = pc_plus_1;
    depth = depth + dpush - dpop;       // debug
    rdepth = rdepth + rpush - rpop;       // debug
end

always @(posedge clk)
begin
    if (reset) begin
        reboot <= 1'b1;
        pc  <= 0;
        st0 <= 0;
        depth = 0;  // debug
        rdepth = 0;  // debug
    end else begin
        reboot <= 0;
        pc <= pcN;
        st0 <= st0N;
    end
end

endmodule
// vim:set shiftwidth=4 softtabstop=4 expandtab:
