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

module j1(
  input wire clk,
  input wire reset,

  output wire io_rd,
  output wire io_wr,
  output wire [15:0] mem_addr,
  output wire mem_wr,
  output wire [`WIDTH-1:0] dout,

  input  wire [`WIDTH-1:0] io_din,

  output wire [12:0] code_addr, // maybe seperate bank
  input  wire [15:0] insn);     // instruction

  reg [3:0] dsp, dspN;          // data stack pointer
  reg [`WIDTH-1:0] st0, st0N;   // top of data stack
  reg dstkW;                    // data stack write

  reg [12:0] pc /* verilator public_flat */, pcN;           // program counter
  wire [12:0] pc_plus_1 = pc + 13'd1;
  reg rstkW;                    // return stack write
  wire [`WIDTH-1:0] rstkD;      // return stack write value
  reg reboot = 1;

  assign mem_addr = st0[15:0];
  assign code_addr = pcN;

  // The D and R stacks
  wire [`WIDTH-1:0] st1, rst0;
  reg [1:0] dspI, rspI;
  stack2 #(.DEPTH(15)) dstack(.clk(clk), .rd(st1),  .we(dstkW), .wd(st0),   .delta(dspI));
  stack2 #(.DEPTH(17)) rstack(.clk(clk), .rd(rst0), .we(rstkW), .wd(rstkD), .delta(rspI));

  wire [16:0] minus = {1'b1, ~st0} + st1 + 1;
  wire signedless = st0[15] ^ st1[15] ? st1[15] : minus[16];

  wire func_T_N =   (insn[6:4] == 1);
  wire func_T_R =   (insn[6:4] == 2);
  wire func_write = (insn[6:4] == 3);
  wire func_iow =   (insn[6:4] == 4);
  wire func_ior =   (insn[6:4] == 5);

  assign is_alu = (insn[15:13] == 3'b011);
  assign is_lit = (insn[15] == 1'b1);
  assign is_branch = (insn[15:13] == 3'b000);
  assign is_branch0 = (insn[15:13] == 3'b001);
  assign is_call = (insn[15:13] == 3'b010);
  assign is_ram_write = (is_alu & insn[5]);     // N->[T]

  assign more = ($signed(st0) > $signed(st1));
  assign equal = (st0 == st1);
  assign umore = ($unsigned(st0) > $unsigned(st1));
  assign zero = (st0 == 0);

  assign we = (is_ram_write and st0[15:14] == 2'b00);

//	dd(0)            <= instruction(0) after delay;
//	rd(0)            <= instruction(2) after delay;
//	dd(dd'high downto 1) <= (others => '1') when instruction(1) = '1' else (others => '0') after delay; -- sign extend
//	rd(rd'high downto 1) <= (others => '1') when instruction(3) = '1' else (others => '0') after delay; -- sign extend
//	dstk_we          <= '1' when (is_instr.lit = '1' or (is_instr.alu = '1' and instruction(7) = '1')) else '0' after delay;
//        <= '1' when is_ram_write = '1' and tos_c(15 downto 14) = "00" else '0' after delay;
  always @*
  begin
    // Compute the new value of st0
    casez ({pc[12], insn[15:8]})
      9'b1_???_?????: st0N = insn;                    // literal
      9'b0_1??_?????: st0N = { {(`WIDTH - 15){1'b0}}, insn[14:0] };    // literal
      9'b0_000_?????: st0N = st0;                     // jump
      9'b0_010_?????: st0N = st0;                     // call
      9'b0_001_?????: st0N = st1;                     // conditional jump
      9'b0_011_?0000: st0N = st0;                     // ALU operations...
      9'b0_011_?0001: st0N = st1;
      9'b0_011_?0010: st0N = st0 + st1;
      9'b0_011_?0011: st0N = st0 & st1;
      9'b0_011_?0100: st0N = st0 | st1;
      9'b0_011_?0101: st0N = st0 ^ st1;
      9'b0_011_?0110: st0N = ~st0;

      9'b0_011_?0111: st0N = {`WIDTH{(minus == 0)}};                //  =
      9'b0_011_?1000: st0N = {`WIDTH{(signedless)}};                //  <

      9'b0_011_?1001: st0N = {st0[`WIDTH - 1], st0[`WIDTH - 1:1]};  // rshift
      9'b0_011_?1010: st0N = {st0[`WIDTH - 2:0], 1'b0};             // lshift
      9'b0_011_?1011: st0N = rst0;                                  // R
      9'b0_011_?1100: st0N = minus[15:0];
      9'b0_011_?1101: st0N = io_din;                                // [T]
      9'b0_011_?1110: st0N = {{(`WIDTH - 4){1'b0}}, dsp};           // depth
      9'b0_011_?1111: st0N = {`WIDTH{(minus[16])}};                 // u<
      default: st0N = {`WIDTH{1'bx}};
    endcase
  end

  assign mem_wr = !reboot & is_alu & func_write;
  assign dout = st1;
  assign io_wr = !reboot & is_alu & func_iow;
  assign io_rd = !reboot & is_alu & func_ior;

  assign rstkD = (insn[13] == 1'b0) ? {{(`WIDTH - 14){1'b0}}, pc_plus_1, 1'b0} : st0;

  always @*
  begin
    casez ({pc[12], insn[15:13]})
    4'b1_???,
    4'b0_1??:   {dstkW, dspI} = {1'b1,      2'b01};
    4'b0_001:   {dstkW, dspI} = {1'b0,      2'b11};
    4'b0_011:   {dstkW, dspI} = {func_T_N,  {insn[1:0]}};
    default:    {dstkW, dspI} = {1'b0,      2'b00};
    endcase
    dspN = dsp + {dspI[1], dspI[1], dspI};

    casez ({pc[12], insn[15:13]})
    4'b1_???:   {rstkW, rspI} = {1'b0,      2'b11};
    4'b0_010:   {rstkW, rspI} = {1'b1,      2'b01};
    4'b0_011:   {rstkW, rspI} = {func_T_R,  insn[3:2]};
    default:    {rstkW, rspI} = {1'b0,      2'b00};
    endcase

    casez ({reboot, pc[12], insn[15:13], insn[7], |st0})
    7'b1_0_???_?_?:   pcN = 0;
    7'b0_0_000_?_?,
    7'b0_0_010_?_?,
    7'b0_0_001_?_0:   pcN = insn[12:0];
    7'b0_1_???_?_?,
    7'b0_0_011_1_?:   pcN = rst0[13:1];
    default:          pcN = pc_plus_1;
    endcase
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
