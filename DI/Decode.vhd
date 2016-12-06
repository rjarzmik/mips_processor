-------------------------------------------------------------------------------
-- Title      : Decode and Issue instruction
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Decode.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-12
-- Last update: 2016-12-06
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Decode and Issue a MIPS instruction
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-12  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;

-------------------------------------------------------------------------------

entity Decode is

  generic (
    ADDR_WIDTH           : integer  := 32;
    DATA_WIDTH           : integer  := 32;
    NB_REGISTERS         : positive := 34;
    NB_REGISTERS_SPECIAL : positive := 2;
    REG_IDX_MFLO         : natural  := 32;
    REG_IDX_MFHI         : natural  := 33
    );

  port (
    clk            : in  std_logic;
    rst            : in  std_logic;
    stall_req      : in  std_logic;     -- stall current instruction
    kill_req       : in  std_logic;     -- kill current instruction
    i_instruction  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_pc           : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    --- Writeback input
    i_rwb_reg1     : in  register_port_type;
    i_rwb_reg2     : in  register_port_type;
    --- Outputs
    o_alu_op       : out alu_op_type;
    o_reg1         : out register_port_type;
    o_reg2         : out register_port_type;
    o_jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_jump_op      : out jump_type;
    o_mem_data     : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_mem_op       : out memory_op_type;
    o_divide_0     : out std_logic;  -- if set, a division attempt will be a X/0
    --- Control outputs
    o_src_reg1_idx : out natural range 0 to NB_REGISTERS - 1;
    o_src_reg2_idx : out natural range 0 to NB_REGISTERS - 1;
    -- Debug signal
    i_dbg_di_pc    : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_di_pc    : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

  constant op_rtype : std_logic_vector(5 downto 0) := "000000";

  constant op_addi  : std_logic_vector(5 downto 0) := "001000";
  constant op_addiu : std_logic_vector(5 downto 0) := "001001";
  constant op_slti  : std_logic_vector(5 downto 0) := "001010";
  constant op_sltiu : std_logic_vector(5 downto 0) := "001011";
  constant op_andi  : std_logic_vector(5 downto 0) := "001100";
  constant op_ori   : std_logic_vector(5 downto 0) := "001101";
  constant op_xori  : std_logic_vector(5 downto 0) := "001110";

  constant op_lui : std_logic_vector(5 downto 0) := "001111";
  constant op_lb  : std_logic_vector(5 downto 0) := "100000";
  constant op_lw  : std_logic_vector(5 downto 0) := "100011";
  constant op_lbu : std_logic_vector(5 downto 0) := "100100";
  constant op_sb  : std_logic_vector(5 downto 0) := "101000";
  constant op_sw  : std_logic_vector(5 downto 0) := "101011";

  constant op_beq  : std_logic_vector(5 downto 0) := "000100";
  constant op_bne  : std_logic_vector(5 downto 0) := "000101";
  constant op_blez : std_logic_vector(5 downto 0) := "000110";
  constant op_bgtz : std_logic_vector(5 downto 0) := "000111";
  constant op_bltz : std_logic_vector(5 downto 0) := "000001";

  constant op_j    : std_logic_vector(5 downto 0) := "000010";
  constant op_jalr : std_logic_vector(5 downto 0) := "000011";

  constant func_nop  : std_logic_vector(5 downto 0) := "000000";
  constant func_mul  : std_logic_vector(5 downto 0) := "011000";
  constant func_mulu : std_logic_vector(5 downto 0) := "011001";
  constant func_div  : std_logic_vector(5 downto 0) := "011010";
  constant func_divu : std_logic_vector(5 downto 0) := "011011";
  constant func_add  : std_logic_vector(5 downto 0) := "100000";
  constant func_addu : std_logic_vector(5 downto 0) := "100001";
  constant func_sub  : std_logic_vector(5 downto 0) := "100010";
  constant func_subu : std_logic_vector(5 downto 0) := "100011";
  constant func_slt  : std_logic_vector(5 downto 0) := "101010";
  constant func_sltu : std_logic_vector(5 downto 0) := "101011";
  constant func_and  : std_logic_vector(5 downto 0) := "100100";
  constant func_or   : std_logic_vector(5 downto 0) := "100101";
  constant func_nor  : std_logic_vector(5 downto 0) := "100111";
  constant func_xor  : std_logic_vector(5 downto 0) := "101000";
  constant func_jr   : std_logic_vector(5 downto 0) := "001000";
  constant func_jalr : std_logic_vector(5 downto 0) := "001001";
  constant func_mfhi : std_logic_vector(5 downto 0) := "010000";
  constant func_mflo : std_logic_vector(5 downto 0) := "010010";

end entity Decode;

-------------------------------------------------------------------------------

architecture rtl of Decode is
  alias ra : std_logic_vector(DATA_WIDTH - 1 downto 0) is o_reg1.data;
  alias rb : std_logic_vector(DATA_WIDTH - 1 downto 0) is o_reg2.data;

  signal alu_op : alu_op_type;

  component RegisterFile is
    generic (
      DATA_WIDTH           : positive;
      NB_REGISTERS         : positive;
      NB_REGISTERS_SPECIAL : positive);
    port (
      clk           : in  std_logic;
      rst           : in  std_logic;
      a_idx         : in  natural range 0 to NB_REGISTERS + NB_REGISTERS_SPECIAL - 1;
      b_idx         : in  natural range 0 to NB_REGISTERS + NB_REGISTERS_SPECIAL - 1;
      rwb_reg1_we   : in  std_logic;
      rwb_reg1_idx  : in  natural range 0 to NB_REGISTERS + NB_REGISTERS_SPECIAL - 1;
      rwb_reg1_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      rwb_reg2_we   : in  std_logic;
      rwb_reg2_idx  : in  natural range 0 to NB_REGISTERS + NB_REGISTERS_SPECIAL - 1;
      rwb_reg2_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      a             : out std_logic_vector(DATA_WIDTH - 1 downto 0);
      b             : out std_logic_vector(DATA_WIDTH - 1 downto 0));
  end component RegisterFile;

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal op_code      : std_logic_vector(5 downto 0);
  signal rsi          : natural range 0 to NB_REGISTERS - 1;
  signal rti          : natural range 0 to NB_REGISTERS - 1;
  signal rdi          : natural range 0 to NB_REGISTERS - 1;
  signal func         : std_logic_vector(5 downto 0);
  signal rs           : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal rt           : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal immediate    : signed(DATA_WIDTH / 2 - 1 downto 0);
  signal next_pc      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal pc_displace  : std_logic_vector(25 downto 0);
  signal rtype        : std_logic;
  signal decode_error : std_logic;
  signal is_immediate : std_logic;
  signal is_branch    : std_logic;
  signal is_jump      : std_logic;
  signal is_rtype     : std_logic;
  signal is_memory    : std_logic;
  -- Converters of register indexes from natural unbound to 0..NB_REGISTERS-1 range
  signal o_reg1_idx   : natural range 0 to NB_REGISTERS - 1;
  signal o_reg2_idx   : natural range 0 to NB_REGISTERS - 1;
  -- Enable writeback of register file if not stalled
  signal rwb_reg1_we  : std_logic;
  signal rwb_reg2_we  : std_logic;

  -----------------------------------------------------------------------------
  -- Internal decoder procedures
  -----------------------------------------------------------------------------
  procedure do_reset(
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal reg2_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal o_divide_0   : out std_logic) is
  begin
    decode_error <= '0';
    alu_op       <= all_zero;
    ra           <= (others => '0');
    rb           <= (others => '0');
    reg1_we      <= '0';
    reg1_idx     <= 0;
    reg2_we      <= '0';
    reg2_idx     <= 0;
    jump_op      <= none;
    mem_op       <= none;
    o_divide_0   <= '1';
  end procedure do_reset;

  procedure do_kill_pipeline_stage(
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal reg2_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal o_divide_0   : out std_logic) is
  begin
    decode_error <= '0';
    alu_op       <= all_zero;
    ra           <= (others => '0');
    rb           <= (others => '0');
    reg1_we      <= '0';
    reg1_idx     <= 0;
    reg2_we      <= '0';
    reg2_idx     <= 0;
    jump_op      <= none;
    jump_target  <= (others => '0');
    mem_op       <= none;
    o_divide_0   <= '1';
  end procedure do_kill_pipeline_stage;

  procedure do_branch(
    signal op_code      : in  std_logic_vector(5 downto 0);
    signal next_pc      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal immediate    : in  signed(DATA_WIDTH / 2 - 1 downto 0);
    signal rs           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rt           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal o_divide_0   : out std_logic) is
  begin
    decode_error <= '0';
    mem_op       <= none;
    alu_op       <= substract;
    ra           <= rs;
    reg1_we      <= '0';
    reg2_we      <= '0';
    o_divide_0   <= '1';
    if (op_code = op_beq) then
      rb      <= rt;
      jump_op <= zero;
    elsif (op_code = op_bne) then
      rb      <= rt;
      jump_op <= non_zero;
    elsif op_code = op_blez then
      rb      <= std_logic_vector(to_unsigned(0, DATA_WIDTH));
      jump_op <= lesser_or_zero;
    elsif op_code = op_bgtz then
      rb      <= std_logic_vector(to_unsigned(0, DATA_WIDTH));
      jump_op <= greater;
    elsif op_code = op_bltz then
      rb      <= std_logic_vector(to_unsigned(0, DATA_WIDTH));
      jump_op <= lesser;
    else
      rb      <= (others => '0');
      jump_op <= none;
    end if;
    jump_target <= std_logic_vector(unsigned(next_pc) + unsigned(resize(immediate * 4, ADDR_WIDTH)));
    o_divide_0  <= '1';
  end procedure do_branch;

  procedure do_immediate(
    signal op_code      : in  std_logic_vector(5 downto 0);
    signal immediate    : in  signed(DATA_WIDTH / 2 - 1 downto 0);
    signal rs           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rti          : in  natural range 0 to NB_REGISTERS - 1;
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal o_divide_0   : out std_logic) is
  begin
    decode_error                    <= '0';
    jump_op                         <= none;
    mem_op                          <= none;
    ra                              <= rs;
    rb(DATA_WIDTH / 2 - 1 downto 0) <= std_logic_vector(immediate);
    reg1_we                         <= '1';
    reg1_idx                        <= rti;
    reg2_we                         <= '0';
    o_divide_0                      <= '1';
    if (op_code = op_addi) then
      alu_op <= add;
      rb(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <=
        (others => immediate(DATA_WIDTH / 2 - 1));
    elsif (op_code = op_addiu) then
      alu_op <= add;
      rb(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <=
        (others => immediate(DATA_WIDTH / 2 - 1));
    elsif (op_code = op_slti) then
      alu_op <= slt;
      rb(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <=
        (others => immediate(DATA_WIDTH / 2 - 1));
    elsif (op_code = op_sltiu) then
      alu_op <= slt;
      rb(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <=
        (others => immediate(DATA_WIDTH / 2 - 1));
    elsif (op_code = op_andi) then
      alu_op <= log_and;
      rb(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <=
        std_logic_vector(to_unsigned(0, DATA_WIDTH / 2));
    elsif (op_code = op_ori) then
      alu_op <= log_or;
      rb(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <=
        std_logic_vector(to_unsigned(0, DATA_WIDTH / 2));
    elsif (op_code = op_xori) then
      alu_op <= log_xor;
      rb(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <=
        std_logic_vector(to_unsigned(0, DATA_WIDTH / 2));
    end if;
    o_divide_0 <= '1';
  end procedure do_immediate;

  procedure do_rtype(
    signal op_code      : in  std_logic_vector(5 downto 0);
    signal func         : in  std_logic_vector(5 downto 0);
    signal next_pc      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal rs           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rt           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rdi          : in  natural range 0 to NB_REGISTERS - 1;
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal reg2_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal o_divide_0   : out std_logic) is
    constant reg_zero : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
  begin
    mem_op <= none;
    if rt /= reg_zero and (func = func_divu or func = func_div) then
      o_divide_0 <= '0';
    else
      o_divide_0 <= '1';
    end if;

    if func = func_jr then
      decode_error <= '0';
      alu_op       <= all_zero;
      reg1_we      <= '0';
      reg2_we      <= '0';
      jump_target  <= rs;
      jump_op      <= always;
    elsif func = func_jalr then
      decode_error <= '0';
      alu_op       <= add;
      ra           <= next_pc;
      rb           <= (others => '0');
      reg1_we      <= '1';
      reg1_idx     <= NB_REGISTERS - 1;
      reg2_we      <= '0';
      jump_target  <= rs;
      jump_op      <= always;
    elsif func = func_nop then
      decode_error <= '0';
      alu_op       <= all_zero;
      reg1_we      <= '0';
      reg2_we      <= '0';
      jump_op      <= none;
    else
      ra      <= rs;
      rb      <= rt;
      reg1_we <= '1';

      if func = func_mul or func = func_mulu then
        decode_error <= '0';
        reg2_we      <= '1';
        reg1_idx     <= REG_IDX_MFLO;
        reg2_idx     <= REG_IDX_MFHI;
        alu_op       <= multiply;
        jump_op      <= none;
      elsif func = func_div or func = func_divu then
        decode_error <= '0';
        reg2_we      <= '1';
        reg1_idx     <= REG_IDX_MFLO;
        reg2_idx     <= REG_IDX_MFHI;
        alu_op       <= divide;
        jump_op      <= none;
      elsif func = func_add or func = func_addu then
        decode_error <= '0';
        reg1_idx     <= rdi;
        reg2_we      <= '0';
        alu_op       <= add;
        jump_op      <= none;
      elsif func = func_sub or func = func_subu then
        decode_error <= '0';
        reg1_idx     <= rdi;
        reg2_we      <= '0';
        alu_op       <= substract;
        jump_op      <= none;
      elsif func = func_slt or func = func_sltu then
        decode_error <= '0';
        reg1_idx     <= rti;
        reg2_we      <= '0';
        alu_op       <= slt;
        jump_op      <= none;
      elsif func = func_and then
        decode_error <= '0';
        reg1_idx     <= rdi;
        reg2_we      <= '0';
        alu_op       <= log_and;
        jump_op      <= none;
      elsif func = func_or then
        decode_error <= '0';
        reg1_idx     <= rdi;
        reg2_we      <= '0';
        alu_op       <= log_or;
        jump_op      <= none;
      elsif func = func_nor then
        decode_error <= '0';
        reg1_idx     <= rdi;
        reg2_we      <= '0';
        alu_op       <= log_nor;
        jump_op      <= none;
      elsif func = func_xor then
        decode_error <= '0';
        reg1_idx     <= rdi;
        reg2_we      <= '0';
        alu_op       <= log_xor;
        jump_op      <= none;
      else
        decode_error <= '1';
      end if;

    end if;

  end procedure do_rtype;

  procedure do_jump(
    signal op_code      : in  std_logic_vector(5 downto 0);
    signal next_pc      : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal pc_displace  : in  std_logic_vector(25 downto 0);
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal o_divide_0   : out std_logic) is
  begin
    decode_error <= '0';
    reg2_we      <= '0';
    jump_op      <= always;
    mem_op       <= none;
    o_divide_0   <= '1';
    jump_target  <= next_pc(ADDR_WIDTH -1 downto pc_displace'length) & pc_displace;
    if op_code = op_jalr then
      alu_op   <= add;
      ra       <= next_pc;
      rb       <= (others => '0');
      reg1_we  <= '1';
      reg1_idx <= NB_REGISTERS - 1;
    else
      alu_op  <= all_zero;
      reg1_we <= '0';
    end if;
  end procedure do_jump;

  procedure do_memory(
    signal op_code      : in  std_logic_vector(5 downto 0);
    signal rs           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rt           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rti          : in  natural range 0 to NB_REGISTERS - 1;
    signal immediate    : in  signed(DATA_WIDTH / 2 - 1 downto 0);
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal mem_data     : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal o_divide_0   : out std_logic) is
  begin
    decode_error <= '0';
    reg2_we      <= '0';
    jump_op      <= none;
    o_divide_0   <= '1';
    if op_code = op_lw then
      mem_op   <= loadw;
      alu_op   <= add;
      ra       <= rs;
      rb       <= std_logic_vector(resize(immediate, DATA_WIDTH));
      reg1_we  <= '1';
      reg1_idx <= rti;
    elsif op_code = op_lbu then
      mem_op   <= load8;
      alu_op   <= add;
      ra       <= rs;
      rb       <= std_logic_vector(resize(immediate, DATA_WIDTH));
      reg1_we  <= '1';
      reg1_idx <= rti;
    elsif op_code = op_lb then
      mem_op   <= load8_signextend32;
      alu_op   <= add;
      ra       <= rs;
      rb       <= std_logic_vector(resize(immediate, DATA_WIDTH));
      reg1_we  <= '1';
      reg1_idx <= rti;
    elsif op_code = op_sw then
      mem_op   <= storew;
      alu_op   <= add;
      ra       <= rs;
      rb       <= std_logic_vector(resize(immediate, DATA_WIDTH));
      reg1_we  <= '0';
      mem_data <= rt;
    elsif op_code = op_sb then
      mem_op   <= store8;
      alu_op   <= add;
      ra       <= rs;
      rb       <= std_logic_vector(resize(immediate, DATA_WIDTH));
      reg1_we  <= '0';
      mem_data <= rt;
    end if;
  end procedure do_memory;

  procedure do_lui(
    signal rt           : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal immediate    : in  signed(DATA_WIDTH / 2 - 1 downto 0);
    signal decode_error : out std_logic;
    signal alu_op       : out alu_op_type;
    signal ra           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal rb           : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal reg1_we      : out std_logic;
    signal reg1_idx     : out natural range 0 to NB_REGISTERS - 1;
    signal reg2_we      : out std_logic;
    signal jump_target  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal jump_op      : out jump_type;
    signal mem_op       : out memory_op_type;
    signal o_divide_0   : out std_logic) is
  begin
    decode_error                             <= '0';
    jump_op                                  <= none;
    alu_op                                   <= add;
    ra(DATA_WIDTH - 1 downto DATA_WIDTH / 2) <= std_logic_vector(immediate);
    ra(DATA_WIDTH / 2 - 1 downto 0)          <= (others => '0');
    rb                                       <= (others => '0');
    reg1_we                                  <= '1';
    reg1_idx                                 <= rti;
    reg2_we                                  <= '0';
    o_divide_0                               <= '1';
  end procedure do_lui;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  rfile : RegisterFile
    generic map (
      DATA_WIDTH           => DATA_WIDTH,
      NB_REGISTERS         => NB_REGISTERS - NB_REGISTERS_SPECIAL,
      NB_REGISTERS_SPECIAL => NB_REGISTERS_SPECIAL)
    port map (
      clk           => clk,
      rst           => rst,
      a_idx         => rsi,
      b_idx         => rti,
      a             => rs,
      b             => rt,
      rwb_reg1_we   => rwb_reg1_we,
      rwb_reg1_idx  => i_rwb_reg1.idx,
      rwb_reg1_data => i_rwb_reg1.data,
      rwb_reg2_we   => rwb_reg2_we,
      rwb_reg2_idx  => i_rwb_reg2.idx,
      rwb_reg2_data => i_rwb_reg2.data
      );

  next_pc <= std_logic_vector(unsigned(i_pc) + 4);

  process(rst, clk, stall_req, kill_req, is_branch, is_immediate, is_rtype,
          is_jump, o_reg1_idx, o_reg2_idx, alu_op)
    variable reg_zero : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
  begin
    if rst = '1' then
      do_reset(decode_error, alu_op, ra, rb, o_reg1.we, o_reg1_idx, o_reg2.we,
               o_reg2_idx, o_jump_target, o_jump_op, o_mem_op, o_divide_0);
    elsif kill_req = '1' and rising_edge(clk) then
      do_kill_pipeline_stage(decode_error, alu_op, ra, rb, o_reg1.we, o_reg1_idx,
                             o_reg2.we, o_reg2_idx, o_jump_target, o_jump_op,
                             o_mem_op, o_divide_0);
    elsif stall_req = '0' and rising_edge(clk) then
      if is_branch = '1' then
        do_branch(op_code, next_pc, immediate, rs, rt,
                  decode_error, alu_op, ra, rb, o_reg1.we, o_reg1_idx,
                  o_reg2.we, o_jump_target, o_jump_op, o_mem_op, o_divide_0);
      elsif is_immediate = '1' then
        do_immediate(op_code, immediate, rs, rti,
                     decode_error, alu_op, ra, rb, o_reg1.we, o_reg1_idx,
                     o_reg2.we, o_jump_target, o_jump_op, o_mem_op, o_divide_0);
      elsif is_rtype = '1' then
        do_rtype(op_code, func, next_pc, rs, rt, rdi,
                 decode_error, alu_op, ra, rb, o_reg1.we, o_reg1_idx,
                 o_reg2.we, o_reg2_idx, o_jump_target, o_jump_op, o_mem_op,
                 o_divide_0);
      elsif is_jump = '1' then
        do_jump(op_code, next_pc, pc_displace, decode_error, alu_op,
                ra, rb, o_reg1.we, o_reg1_idx, o_reg2.we, o_jump_target,
                o_jump_op, o_mem_op, o_divide_0);
      elsif is_memory = '1' then
        do_memory(op_code, rs, rt, rti, immediate, decode_error, alu_op,
                  ra, rb, o_reg1.we, o_reg1.idx, o_reg2.we, o_jump_target,
                  o_jump_op, o_mem_op, o_mem_data, o_divide_0);
      elsif op_code = op_lui then
        do_lui(rt, immediate, decode_error, alu_op,
               ra, rb, o_reg1.we, o_reg1_idx, o_reg2.we, o_jump_target,
               o_jump_op, o_mem_op, o_divide_0);
      end if;
    end if;

    o_reg1.idx <= o_reg1_idx;
    o_reg2.idx <= o_reg2_idx;
    o_alu_op   <= alu_op;
  end process;

  debug : process(rst, clk, stall_req, kill_req)
  begin
    if rst = '1' then
      o_dbg_di_pc <= (others => 'X');
    elsif rising_edge(clk) and kill_req = '1' then
      o_dbg_di_pc <= (others => 'X');
    elsif rising_edge(clk) and stall_req = '1' then
    elsif rising_edge(clk) then
      o_dbg_di_pc <= i_dbg_di_pc;
    end if;
  end process debug;

  op_code <= i_instruction(31 downto 26);
  func    <= i_instruction(5 downto 0);

  rsi          <= to_integer(unsigned(i_instruction(25 downto 21)));
  rti          <= to_integer(unsigned(i_instruction(20 downto 16)));
  rdi          <= to_integer(unsigned(i_instruction(15 downto 11)));
  immediate    <= signed(i_instruction(15 downto 0));
  pc_displace  <= i_instruction(23 downto 0) & b"00";
  is_immediate <= '1' when (op_code >= op_addi and op_code <= op_xori) or
                  (op_code = op_lui) or
                  (op_code = op_lb) or
                  (op_code = op_lw) or
                  (op_code = op_lbu) or
                  (op_code = op_sb) or
                  (op_code = op_sw) else '0';
  is_branch <= '1' when
               (op_code = op_beq) or
               (op_code = op_bne) or
               (op_code = op_blez) or
               (op_code = op_bgtz) or
               (op_code = op_bltz) else '0';
  is_rtype  <= '1' when (op_code = op_rtype)                  else '0';
  is_jump   <= '1' when (op_code = op_j or op_code = op_jalr) else '0';
  is_memory <= '1' when (op_code = op_lui) or
               (op_code = op_lb) or
               (op_code = op_lw) or
               (op_code = op_lbu) or
               (op_code = op_sb) or
               (op_code = op_sw) else '0';

  rwb_reg1_we <= i_rwb_reg1.we;
  rwb_reg2_we <= i_rwb_reg2.we;

  o_src_reg1_idx <= rsi;
  o_src_reg2_idx <= rti;

end architecture rtl;

-------------------------------------------------------------------------------
