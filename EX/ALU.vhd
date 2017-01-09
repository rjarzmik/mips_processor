-------------------------------------------------------------------------------
-- Title      : Arithmetic and Logic Unit
-- Project    : 
-------------------------------------------------------------------------------
-- File       : ALU.vhd.vhd
-- Author     : Robert Jarzmik (Intel)  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-16
-- Last update: 2017-01-09
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
    o_ready       : out std_logic;
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

architecture rtl of ALU is
  signal ra : unsigned(DATA_WIDTH - 1 downto 0);
  signal rb : unsigned(DATA_WIDTH - 1 downto 0);

  signal nstall_req : std_logic;
  signal q          : unsigned(DATA_WIDTH * 2 - 1 downto 0) := (others => '0');
  signal jump_op    : jump_type;

  signal alu_op_q      : alu_op_type;
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
  nstall_req <= not stall_req;

  adder : entity work.ALU_Adder
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra   => ra,
      i_rb   => rb,
      o_q    => adder_q);

  substracter : entity work.ALU_Substracter
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra   => ra,
      i_rb   => rb,
      o_q    => substracter_q);

  multiplier : entity work.ALU_Multiplier
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra => ra,
      i_rb => rb,
      o_q  => multiplier_q);

  divider : entity work.ALU_Divider
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra       => ra,
      i_rb       => rb,
      i_div_by_0 => i_divide_0,
      o_q        => divider_q);
  --divider_q(DATA_WIDTH - 1 downto 0)              <= std_logic_vector(to_unsigned(1, divider_q'length / 2));
  --divider_q(DATA_WIDTH * 2 - 1 downto DATA_WIDTH) <= std_logic_vector(to_unsigned(1, divider_q'length / 2));

  do_log_and : entity work.ALU_Log_And
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra   => ra,
      i_rb   => rb,
      o_q    => log_and_q);

  do_log_or : entity work.ALU_Log_or
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra   => ra,
      i_rb   => rb,
      o_q    => log_or_q);

  do_log_nor : entity work.ALU_Log_nor
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra   => ra,
      i_rb   => rb,
      o_q    => log_nor_q);

  do_log_xor : entity work.ALU_Log_xor
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra   => ra,
      i_rb   => rb,
      o_q    => log_xor_q);

  do_slt : entity work.ALU_Set_Lower_Than
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk    => clk,
      clkena => nstall_req,
      i_ra   => ra,
      i_rb   => rb,
      o_q    => slt_q);

  ALU_Mux_1 : entity work.ALU_Mux
    generic map (
      DATA_WIDTH => DATA_WIDTH)
    port map (
      i_alu_op    => alu_op_q,
      i_add       => std_logic_vector(adder_q),
      i_sub       => std_logic_vector(substracter_q),
      i_mul       => std_logic_vector(multiplier_q),
      i_div       => std_logic_vector(divider_q),
      i_and       => std_logic_vector(log_and_q),
      i_or        => std_logic_vector(log_or_q),
      i_xor       => std_logic_vector(log_xor_q),
      i_nor       => std_logic_vector(log_nor_q),
      i_slt       => std_logic_vector(slt_q),
      unsigned(q) => q);

  o_q_sel : process(rst, clk, stall_req, kill_req, q)
  begin
    o_reg1.data <= std_logic_vector(q(DATA_WIDTH -1 downto 0));
    o_reg2.data <= std_logic_vector(q(DATA_WIDTH * 2 -1 downto DATA_WIDTH));

    if rst = '1' then
      alu_op_q <= all_zero;
    elsif rising_edge(clk) then
      if kill_req = '1' then
      elsif stall_req = '1' then
      else
        alu_op_q <= alu_op;
      end if;
    end if;
  end process o_q_sel;

  ready : process(rst, clk)
    variable shifter : std_logic_vector(3 downto 0);
  begin
    if rst = '1' then
      shifter := b"1111";
    elsif rising_edge(clk) then
      if alu_op = multiply or alu_op = divide then
        shifter := b"0000";
      else
        shifter := b"1" & shifter(3 downto 1);
      end if;
    end if;
    o_ready <= shifter(0);
  end process ready;

  process(rst, clk)
    variable itag : instr_tag_t;
  begin
    if rst = '1' then
      o_reg1.we         <= '0';
      o_reg2.we         <= '0';
      o_mem_op          <= none;
      o_instr_tag.valid <= false;
    else
      if rising_edge(clk) then
        if kill_req = '1' then
          o_reg1.we   <= '0';
          o_reg2.we   <= '0';
          o_mem_op    <= none;
          itag.valid  := false;
          o_instr_tag <= itag;
        elsif stall_req = '1' then
        else
          o_reg1.we     <= i_reg1.we;
          o_reg1.idx    <= i_reg1.idx;
          o_reg2.we     <= i_reg2.we;
          o_reg2.idx    <= i_reg2.idx;
          o_jump_target <= i_jump_target;
          o_mem_data    <= i_mem_data;
          o_mem_op      <= i_mem_op;

          itag        := i_instr_tag;
          o_instr_tag <= itag;
        end if;
      end if;
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

  jumper : process(rst, clk, stall_req, kill_req)
    variable jump                      : jump_type := none;
    variable is_eq, is_lesser, do_jump : boolean;
  begin
    if rst = '1' then
      jump := none;
    elsif rising_edge(clk) then
      if kill_req = '1' then
        jump := none;
      elsif stall_req = '1' then
      else
        is_eq     := (ra = rb);
        is_lesser := (ra < rb);
        case jump_op is
          when always | none   => do_jump := (jump_op = always);
          when zero            => do_jump := is_eq;
          when non_zero        => do_jump := not is_eq;
          when lesser_or_zero  => do_jump := is_lesser or is_eq;
          when lesser          => do_jump := is_lesser;
          when greater         => do_jump := not is_lesser and not is_eq;
          when greater_or_zero => do_jump := not is_lesser;
        end case;
      end if;
    end if;

    if do_jump then
      o_is_jump <= '1';
    else
      o_is_jump <= '0';
    end if;
  end process jumper;

  ra      <= (others => '0') when rst = '1' else unsigned(i_reg1.data);
  rb      <= (others => '0') when rst = '1' else unsigned(i_reg2.data);
  jump_op <= i_jump_op;

end architecture rtl;
