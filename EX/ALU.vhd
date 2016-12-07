-------------------------------------------------------------------------------
-- Title      : Arithmetic and Logic Unit
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ALU.vhd.vhd
-- Author     : Robert Jarzmik (Intel)  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-16
-- Last update: 2016-12-07
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Integer Computing Unit
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-16  1.0      rjarzmik        Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.instruction_defs.all;

-------------------------------------------------------------------------------

entity ALU is

  generic (
    ADDR_WIDTH   : integer  := 32;
    DATA_WIDTH   : integer  := 32;
    NB_REGISTERS : positive := 32
    );

  port (
    clk           : in  std_logic;
    rst           : in  std_logic;
    stall_req     : in  std_logic;      -- stall current instruction
    kill_req      : in  std_logic;      -- kill current instruction
    alu_op        : in  alu_op_type;
    i_reg1        : in  register_port_type;
    i_reg2        : in  register_port_type;
    i_divide_0    : in  std_logic;  -- if set, a division attempt will be a X/0
    -- Carry-over signals
    i_jump_target : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_jump_op     : in  jump_type;
    i_mem_data    : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_mem_op      : in  memory_op_type;
    i_instr_tag   : in  instr_tag_t;
    o_reg1        : out register_port_type;
    o_reg2        : out register_port_type;
    o_jump_target : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_is_jump     : out std_logic;
    o_mem_data    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_mem_op      : out memory_op_type;
    o_instr_tag   : out instr_tag_t;
    -- Debug signal
    i_dbg_ex_pc   : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_ex_pc   : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity ALU;

-------------------------------------------------------------------------------

architecture rtl of ALU is
  signal ra : unsigned(DATA_WIDTH - 1 downto 0);
  signal rb : unsigned(DATA_WIDTH - 1 downto 0);

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal q          : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal cond_zero  : std_logic;
  signal cond_carry : std_logic;
  signal jump_op    : jump_type;
  signal is_jump    : std_logic;

  signal adder_q       : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal substracter_q : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal multiplier_q  : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal divider_q     : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal log_and_q     : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal log_or_q      : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal log_nor_q     : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal log_xor_q     : unsigned(DATA_WIDTH * 2 - 1 downto 0);
  signal slt_q         : unsigned(DATA_WIDTH * 2 - 1 downto 0);


begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  adder : entity work.ALU_Adder
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => adder_q);

  substracter : entity work.ALU_Substracter
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => substracter_q);

  multiplier : entity work.ALU_Multiplier
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => multiplier_q);

  divider : entity work.ALU_Divider
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra       => ra,
      i_rb       => rb,
      i_div_by_0 => i_divide_0,
      o_q        => divider_q);

  do_log_and : entity work.ALU_Log_And
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => log_and_q);

  do_log_or : entity work.ALU_Log_or
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => log_or_q);

  do_log_nor : entity work.ALU_Log_nor
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => log_nor_q);

  do_log_xor : entity work.ALU_Log_xor
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => log_xor_q);

  do_slt : entity work.ALU_Set_Lower_Than
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_ra => ra,
      i_rb => rb,
      o_q  => slt_q);

  with alu_op select q <=
    adder_q       when add,
    substracter_q when substract,
    multiplier_q  when multiply,
    divider_q     when divide,
    log_and_q     when log_and,
    log_or_q      when log_or,
    log_nor_q     when log_nor,
    log_xor_q     when log_xor,
    slt_q         when slt,
    adder_q       when all_zero;

  process(rst, clk, kill_req, stall_req)
  begin
    if rst = '1' and rising_edge(clk) then
      o_reg1.we   <= '0';
      o_reg2.we   <= '0';
      o_is_jump   <= '0';
      o_mem_op    <= none;
      o_instr_tag <= INSTR_TAG_NONE;
    elsif kill_req = '1' and rising_edge(clk) then
      o_reg1.we   <= '0';
      o_reg2.we   <= '0';
      o_is_jump   <= '0';
      o_mem_op    <= none;
      o_instr_tag <= INSTR_TAG_NONE;
    elsif stall_req = '0' and rising_edge(clk) then
      o_reg1.we     <= i_reg1.we;
      o_reg1.idx    <= i_reg1.idx;
      o_reg2.we     <= i_reg2.we;
      o_reg2.idx    <= i_reg2.idx;
      o_jump_target <= i_jump_target;
      o_mem_data    <= i_mem_data;
      o_mem_op      <= i_mem_op;
      o_is_jump     <= is_jump;

      o_reg1.data <= std_logic_vector(q(DATA_WIDTH -1 downto 0));
      o_reg2.data <= std_logic_vector(q(DATA_WIDTH * 2 -1 downto DATA_WIDTH));
      o_instr_tag <= i_instr_tag;
    end if;
  end process;

  debug : process(rst, clk, stall_req, kill_req)
  begin
    if rst = '1' then
      o_dbg_ex_pc <= (others => 'X');
    elsif rising_edge(clk) and kill_req = '1' then
      o_dbg_ex_pc <= (others => 'X');
    elsif rising_edge(clk) and stall_req = '1' then
    elsif rising_edge(clk) then
      o_dbg_ex_pc <= i_dbg_ex_pc;
    end if;
  end process debug;

  ra         <= unsigned(i_reg1.data);
  rb         <= unsigned(i_reg2.data);
  cond_zero  <= '1' when unsigned(q) = to_unsigned(0, q'length) else '0';
  cond_carry <= q(DATA_WIDTH);
  jump_op    <= i_jump_op;

  is_jump <= '1' when
             (jump_op = always) or
             (jump_op = zero and cond_zero = '1') or
             (jump_op = non_zero and cond_zero = '0') or
             (jump_op = lesser_or_zero and (cond_carry = '1' or cond_zero = '1')) or
             (jump_op = lesser and cond_carry = '1') or
             (jump_op = greater and cond_carry = '0') or
             (jump_op = greater_or_zero and (cond_carry = '0' or cond_zero = '1'))
             else '0';

end architecture rtl;

-------------------------------------------------------------------------------
