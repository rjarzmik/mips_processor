-------------------------------------------------------------------------------
-- Title      : MIPS Processor
-- Project    : 
-------------------------------------------------------------------------------
-- File       : MIPS_CPU.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-11
-- Last update: 2016-12-14
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: A MIPS v1 processor, not pipelined
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-11  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;

-------------------------------------------------------------------------------

entity MIPS_CPU is

  generic (
    ADDR_WIDTH           : integer  := 32;
    DATA_WIDTH           : integer  := 32;
    NB_REGISTERS_GP      : positive := 32;  -- r0 to r31
    NB_REGISTERS_SPECIAL : positive := 2    -- mflo and mfhi
    );

  port (
    clk             : in  std_logic;
    rst             : in  std_logic;
    -- L2 cache lines
    o_L2c_req       : out std_logic;
    o_L2c_addr      : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_L2c_read_data : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_L2c_valid     : in  std_logic;

    -- Temprorary Data Memory interface
    o_mem_addr       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_mem_rd_valid   : in  std_logic;
    i_mem_rd_data    : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_mem_wr_en      : out std_logic;
    o_mem_word_width : out std_logic;
    o_mem_wr_data    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_mem_wr_ack     : in  std_logic;

    -- Debug signals
    signal o_dbg_if_pc       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_di_pc       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_ex_pc       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_mem_pc      : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_wb_pc       : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_commited_pc : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity MIPS_CPU;

-------------------------------------------------------------------------------

architecture rtl of MIPS_CPU is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal current_pc          : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal next_pc             : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal next_next_pc        : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal jump_target         : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal fetched_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal fetched_pc          : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal fetch_stalls_pc     : std_logic;
  signal alu_op              : alu_op_type;
  signal di2ex_ra            : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal di2ex_rb            : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal di2ex_reg1          : register_port_type;
  signal di2ex_reg1_we       : std_logic;
  signal di2ex_reg1_idx      : natural range 0 to NB_REGISTERS_GP + NB_REGISTERS_SPECIAL - 1;
  signal di2ex_reg2          : register_port_type;
  signal di2ex_reg2_we       : std_logic;
  signal di2ex_reg2_idx      : natural range 0 to NB_REGISTERS_GP + NB_REGISTERS_SPECIAL - 1;
  signal di2ex_divide_0      : std_logic;
  signal jump_pc             : std_logic;
  signal di2ctrl_reg1_idx    : natural range 0 to NB_REGISTERS_GP + NB_REGISTERS_SPECIAL - 1;
  signal di2ctrl_reg2_idx    : natural range 0 to NB_REGISTERS_GP + NB_REGISTERS_SPECIAL - 1;
  signal wb2di_reg1          : register_port_type;
  signal wb2di_reg2          : register_port_type;
  signal di2ex_jump_target   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal di2ex_jump_op       : jump_type;
  signal di2ex_mem_data      : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal di2ex_mem_op        : memory_op_type;

  signal ex2mem_reg1        : register_port_type;
  signal ex2mem_reg2        : register_port_type;
  signal ex2mem_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal ex2mem_is_jump     : std_logic;
  signal ex2mem_mem_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal ex2mem_mem_op      : memory_op_type;

  signal mem2ctrl_stage1_reg1 : register_port_type;
  signal mem2ctrl_stage1_reg2 : register_port_type;
  signal mem2ctrl_stage2_reg1 : register_port_type;
  signal mem2ctrl_stage2_reg2 : register_port_type;

  signal mem2wb_reg1        : register_port_type;
  signal mem2wb_reg2        : register_port_type;
  signal mem2wb_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal mem2wb_is_jump     : std_logic;

  signal wb_is_jump        : std_logic;
  signal wb_jump_target    : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal wb_kills_pipeline : std_logic;

  -- Control signals
  --- Dependencies checkers
  signal RaW_detected           : std_logic;
  signal mem2upstream_stall_req : std_logic;
  --- Exception signals
  signal mem_exception          : std_logic;
  --- Pipeline stage stallers
  signal pc_stalled             : std_logic;
  signal ife_stalled            : std_logic;
  signal di_stalled             : std_logic;
  signal ex_stalled             : std_logic;
  signal mem_stalled            : std_logic;
  signal wb_stalled             : std_logic;
  --- Pipeline stage output killers (ie. "nop" replacement of stage output)
  signal di_killed              : std_logic;
  signal ex_killed              : std_logic;
  signal mem_killed             : std_logic;
  signal wb_killed              : std_logic;

  -- Debug signals
  signal dbg_if_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_di_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_ex_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_mem_pc      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_wb_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_commited_pc : std_logic_vector(ADDR_WIDTH - 1 downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  pc_reg : entity work.PC_Register
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => 4)
    port map (
      clk            => clk,
      rst            => rst,
      stall_pc       => pc_stalled,
      jump_pc        => wb_is_jump,
      jump_target    => wb_jump_target,
      o_current_pc   => current_pc,
      o_next_pc      => next_pc,
      o_next_next_pc => next_next_pc);

  ife : entity work.Fetch(rtl3)
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk                  => clk,
      rst                  => rst,
      stall_req            => ife_stalled,
      kill_req             => wb_is_jump,
      i_pc                 => current_pc,
      i_next_pc            => next_pc,
      i_next_next_pc       => next_next_pc,
      o_instruction        => fetched_instruction,
      o_pc_instr           => fetched_pc,
      o_do_stall_pc        => fetch_stalls_pc,
      o_L2c_req            => o_L2c_req,
      o_L2c_addr           => o_L2c_addr,
      i_L2c_read_data      => i_L2c_read_data,
      i_L2c_valid          => i_L2c_valid,
      o_dbg_if_fetching_pc => dbg_if_pc,
      o_dbg_if_pc          => dbg_di_pc);

  di : entity work.Decode
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      NB_REGISTERS         => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL,
      NB_REGISTERS_SPECIAL => NB_REGISTERS_SPECIAL,
      REG_IDX_MFLO         => 32,
      REG_IDX_MFHI         => 33)
    port map (
      clk            => clk,
      rst            => rst,
      stall_req      => di_stalled,
      kill_req       => di_killed,
      i_instruction  => fetched_instruction,
      i_pc           => fetched_pc,
      i_rwb_reg1     => wb2di_reg1,
      i_rwb_reg2     => wb2di_reg2,
      o_alu_op       => alu_op,
      o_reg1         => di2ex_reg1,
      o_reg2         => di2ex_reg2,
      o_jump_target  => di2ex_jump_target,
      o_jump_op      => di2ex_jump_op,
      o_mem_data     => di2ex_mem_data,
      o_mem_op       => di2ex_mem_op,
      o_divide_0     => di2ex_divide_0,
      o_src_reg1_idx => di2ctrl_reg1_idx,
      o_src_reg2_idx => di2ctrl_reg2_idx,
      i_dbg_di_pc    => dbg_di_pc,
      o_dbg_di_pc    => dbg_ex_pc);

  ex : entity work.ALU
    generic map (
      ADDR_WIDTH   => ADDR_WIDTH,
      DATA_WIDTH   => DATA_WIDTH,
      NB_REGISTERS => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL)
    port map (
      clk           => clk,
      rst           => rst,
      stall_req     => ex_stalled,
      kill_req      => ex_killed,
      alu_op        => alu_op,
      i_reg1        => di2ex_reg1,
      i_reg2        => di2ex_reg2,
      i_jump_target => di2ex_jump_target,
      i_jump_op     => di2ex_jump_op,
      i_mem_data    => di2ex_mem_data,
      i_mem_op      => di2ex_mem_op,
      i_divide_0    => di2ex_divide_0,
      o_reg1        => ex2mem_reg1,
      o_reg2        => ex2mem_reg2,
      o_jump_target => ex2mem_jump_target,
      o_is_jump     => ex2mem_is_jump,
      o_mem_data    => ex2mem_mem_data,
      o_mem_op      => ex2mem_mem_op,
      i_dbg_ex_pc   => dbg_ex_pc,
      o_dbg_ex_pc   => dbg_mem_pc);

  mem_stage : entity work.Memory_access
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk         => clk,               -- input clk
      rst         => rst,               -- input async reset
      stall_req   => mem_stalled,
      kill_req    => mem_killed,
      o_exception => mem_exception,

      i_reg1        => ex2mem_reg1,
      i_reg2        => ex2mem_reg2,
      i_mem_op      => ex2mem_mem_op,
      i_mem_data    => ex2mem_mem_data,
      i_is_jump     => ex2mem_is_jump,
      i_jump_target => ex2mem_jump_target,

      o_reg1        => mem2wb_reg1,
      o_reg2        => mem2wb_reg2,
      o_jump_target => mem2wb_jump_target,
      o_is_jump     => mem2wb_is_jump,

      o_stage1_reg1 => mem2ctrl_stage1_reg1,
      o_stage1_reg2 => mem2ctrl_stage1_reg2,
      o_stage2_reg1 => mem2ctrl_stage2_reg1,
      o_stage2_reg2 => mem2ctrl_stage2_reg2,

      -- Memory interface
      i_mem_rd_valid   => i_mem_rd_valid,
      i_mem_rd_data    => i_mem_rd_data,
      o_mem_wr_en      => o_mem_wr_en,
      o_mem_word_width => o_mem_word_width,

      i_mem_wr_ack  => i_mem_wr_ack,
      o_mem_addr    => o_mem_addr,
      o_mem_wr_data => o_mem_wr_data,
      o_need_stall  => mem2upstream_stall_req,

      -- debug signals
      i_dbg_mem_pc => dbg_mem_pc,
      o_dbg_mem_pc => dbg_wb_pc);

  wb : entity work.Writeback
    generic map (
      ADDR_WIDTH   => ADDR_WIDTH,
      DATA_WIDTH   => DATA_WIDTH,
      NB_REGISTERS => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL)
    port map (
      clk           => clk,
      rst           => rst,
      stall_req     => wb_stalled,
      kill_req      => wb_killed,
      i_reg1        => mem2wb_reg1,
      i_reg2        => mem2wb_reg2,
      i_jump_target => mem2wb_jump_target,
      i_is_jump     => mem2wb_is_jump,
      o_reg1        => wb2di_reg1,
      o_reg2        => wb2di_reg2,
      o_is_jump     => wb_is_jump,
      o_jump_target => wb_jump_target,
      i_dbg_wb_pc   => dbg_wb_pc,
      o_dbg_wb_pc   => dbg_commited_pc);

  ctrl_decode_deps : entity work.Control_Decode_Dependencies
    generic map (
      NB_REGISTERS => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL)
    port map (
      clk           => clk,
      rst           => rst,
      rsi           => di2ctrl_reg1_idx,
      rti           => di2ctrl_reg2_idx,
      i_di2ex_reg1  => di2ex_reg1,
      i_di2ex_reg2  => di2ex_reg2,
      i_ex2mem_reg1 => ex2mem_reg1,
      i_ex2mem_reg2 => ex2mem_reg2,

      i_mem2ctrl_stage1_reg1 => mem2ctrl_stage1_reg1,
      i_mem2ctrl_stage1_reg2 => mem2ctrl_stage1_reg2,
      i_mem2ctrl_stage2_reg1 => mem2ctrl_stage2_reg1,
      i_mem2ctrl_stage2_reg2 => mem2ctrl_stage2_reg2,

      i_mem2wb_reg1 => mem2wb_reg1,
      i_mem2wb_reg2 => mem2wb_reg2,

      i_wb2di_reg1   => wb2di_reg1,
      i_wb2di_reg2   => wb2di_reg2,
      o_raw_detected => RaW_detected);

  wb_kills_pipeline <= wb_is_jump;

  -- Control signals
  pc_stalled  <= fetch_stalls_pc or mem2upstream_stall_req;
  ife_stalled <= RaW_detected or mem2upstream_stall_req;
  di_stalled  <= mem2upstream_stall_req;
  ex_stalled  <= mem2upstream_stall_req;
  mem_stalled <= '0';
  wb_stalled  <= '0';

  di_killed  <= wb_kills_pipeline or RaW_detected;
  ex_killed  <= wb_kills_pipeline;
  mem_killed <= wb_kills_pipeline;
  wb_killed  <= wb_kills_pipeline;

  -- Debug signal
  o_dbg_if_pc       <= dbg_if_pc;
  o_dbg_di_pc       <= dbg_di_pc;
  o_dbg_ex_pc       <= dbg_ex_pc;
  o_dbg_mem_pc      <= dbg_mem_pc;
  o_dbg_wb_pc       <= dbg_wb_pc;
  o_dbg_commited_pc <= dbg_commited_pc;

end architecture rtl;

-------------------------------------------------------------------------------
