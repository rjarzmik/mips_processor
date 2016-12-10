-------------------------------------------------------------------------------
-- Title      : MIPS Processor
-- Project    : 
-------------------------------------------------------------------------------
-- File       : MIPS_CPU.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-11
-- Last update: 2016-12-10
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
use work.instruction_defs.instr_tag_t;

-------------------------------------------------------------------------------

entity MIPS_CPU is

  generic (
    ADDR_WIDTH           : integer  := 32;
    DATA_WIDTH           : integer  := 32;
    NB_REGISTERS_GP      : positive := 32;  -- r0 to r31
    NB_REGISTERS_SPECIAL : positive := 2    -- mflo and mfhi
    );

  port (
    clk                             : in  std_logic;
    rst                             : in  std_logic;
    -- L2 cache lines
    o_L2c_req                       : out std_logic;
    o_L2c_addr                      : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_L2c_read_data                 : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_L2c_valid                     : in  std_logic;
    -- Debug signals
    signal o_dbg_if_pc              : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_di_pc              : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_ex_pc              : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_wb_pc              : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_commited_pc        : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_pc_killed          : out std_logic;
    signal o_dbg_ife_killed         : out std_logic;
    signal o_dbg_di_killed          : out std_logic;
    signal o_dbg_ex_killed          : out std_logic;
    signal o_dbg_wb_killed          : out std_logic;
    signal o_dbg_pc_stalled         : out std_logic;
    signal o_dbg_ife_stalled        : out std_logic;
    signal o_dbg_di_stalled         : out std_logic;
    signal o_dbg_ex_stalled         : out std_logic;
    signal o_dbg_wb_stalled         : out std_logic;
    signal o_dbg_jump_pc            : out std_logic;
    signal o_dbg_jump_target        : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    signal o_dbg_commited_instr_tag : out instr_tag_t;
    signal o_dbg_wb2di_reg1         : out register_port_type;
    signal o_dbg_wb2di_reg2         : out register_port_type;
    signal o_dbg_if_instr_tag       : out instr_tag_t;
    signal o_dbg_di_instr_tag       : out instr_tag_t;
    signal o_dbg_ex_instr_tag       : out instr_tag_t;
    signal o_dbg_wb_instr_tag       : out instr_tag_t
    );

end entity MIPS_CPU;

-------------------------------------------------------------------------------

architecture rtl of MIPS_CPU is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal current_pc          : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal next_pc             : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal jump_target         : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal fetched_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal fetched_pc          : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal fetch_stalls_pc     : std_logic;
  signal if_instr_tag        : instr_tag_t;
  signal di_instr_tag        : instr_tag_t;
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
  signal ex_instr_tag        : instr_tag_t;

  signal ex2wb_reg1        : register_port_type;
  signal ex2wb_reg2        : register_port_type;
  signal ex2wb_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal ex2wb_is_jump     : std_logic;
  signal ex2wb_mem_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal ex2wb_mem_op      : memory_op_type;
  signal wb_instr_tag      : instr_tag_t;

  signal wb_is_jump         : std_logic;
  signal wb_jump_target     : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal commited_instr_tag : instr_tag_t;

  -- Control signals
  --- Dependencies checkers
  signal RaW_detected              : std_logic;
  --- Pipeline stage stallers
  signal pc_stalled                : std_logic;
  signal ife_stalled               : std_logic;
  signal di_stalled                : std_logic;
  signal ex_stalled                : std_logic;
  signal wb_stalled                : std_logic;
  --- Pipeline stage output killers (ie. "nop" replacement of stage output)
  signal mispredict_kills_pipeline : std_logic;
  signal pc_killed                 : std_logic;
  signal ife_killed                : std_logic;
  signal di_killed                 : std_logic;
  signal ex_killed                 : std_logic;
  signal wb_killed                 : std_logic;

  -- Debug signals
  signal dbg_if_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_di_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_ex_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_wb_pc       : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal dbg_commited_pc : std_logic_vector(ADDR_WIDTH - 1 downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  ife : entity work.Fetch(rtl3)
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk                         => clk,
      rst                         => rst,
      stall_req                   => ife_stalled,
      kill_req                    => ife_killed,
      o_instruction               => fetched_instruction,
      o_pc_instr                  => fetched_pc,
      o_instr_tag                 => di_instr_tag,
      o_mispredict_kill_pipeline  => mispredict_kills_pipeline,
      o_L2c_req                   => o_L2c_req,
      o_L2c_addr                  => o_L2c_addr,
      i_L2c_read_data             => i_L2c_read_data,
      i_L2c_valid                 => i_L2c_valid,
      i_is_jump                   => wb_is_jump,
      i_jump_target               => wb_jump_target,
      i_commited_instr_tag        => commited_instr_tag,
      o_dbg_if_fetching_pc        => dbg_if_pc,
      o_dbg_if_pc                 => dbg_di_pc,
      o_dbg_if_fetching_instr_tag => if_instr_tag);

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
      i_instr_tag    => di_instr_tag,
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
      o_instr_tag    => ex_instr_tag,
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
      i_instr_tag   => ex_instr_tag,
      i_divide_0    => di2ex_divide_0,
      o_reg1        => ex2wb_reg1,
      o_reg2        => ex2wb_reg2,
      o_jump_target => ex2wb_jump_target,
      o_is_jump     => ex2wb_is_jump,
      o_mem_data    => ex2wb_mem_data,
      o_mem_op      => ex2wb_mem_op,
      o_instr_tag   => wb_instr_tag,
      i_dbg_ex_pc   => dbg_ex_pc,
      o_dbg_ex_pc   => dbg_wb_pc);

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
      i_reg1        => ex2wb_reg1,
      i_reg2        => ex2wb_reg2,
      i_jump_target => ex2wb_jump_target,
      i_is_jump     => ex2wb_is_jump,
      o_reg1        => wb2di_reg1,
      o_reg2        => wb2di_reg2,
      o_is_jump     => wb_is_jump,
      o_jump_target => wb_jump_target,
      i_instr_tag   => wb_instr_tag,
      o_instr_tag   => commited_instr_tag,
      i_dbg_wb_pc   => dbg_wb_pc,
      o_dbg_wb_pc   => dbg_commited_pc);

  ctrl_decode_deps : entity work.Control_Decode_Dependencies
    generic map (
      NB_REGISTERS => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL)
    port map (
      clk            => clk,
      rst            => rst,
      rsi            => di2ctrl_reg1_idx,
      rti            => di2ctrl_reg2_idx,
      i_di2ex_reg1   => di2ex_reg1,
      i_di2ex_reg2   => di2ex_reg2,
      i_ex2wb_reg1   => ex2wb_reg1,
      i_ex2wb_reg2   => ex2wb_reg2,
      i_wb2di_reg1   => wb2di_reg1,
      i_wb2di_reg2   => wb2di_reg2,
      o_raw_detected => RaW_detected);

  -- Control signals
  pc_stalled  <= fetch_stalls_pc;
  ife_stalled <= RaW_detected;
  di_stalled  <= '0';
  ex_stalled  <= '0';
  wb_stalled  <= '0';

  ife_killed <= mispredict_kills_pipeline;
  di_killed  <= mispredict_kills_pipeline or RaW_detected;
  ex_killed  <= mispredict_kills_pipeline;
  wb_killed  <= mispredict_kills_pipeline;

  -- Debug signal
  o_dbg_if_pc <= dbg_if_pc;
  o_dbg_di_pc <= dbg_di_pc;
  o_dbg_ex_pc <= dbg_ex_pc;
  o_dbg_wb_pc <= dbg_wb_pc;

  o_dbg_commited_pc <= dbg_commited_pc;
  o_dbg_pc_killed   <= pc_killed;
  o_dbg_ife_killed  <= ife_killed;
  o_dbg_di_killed   <= di_killed;
  o_dbg_ex_killed   <= ex_killed;
  o_dbg_wb_killed   <= wb_killed;
  o_dbg_pc_stalled  <= pc_stalled;
  o_dbg_ife_stalled <= ife_stalled;
  o_dbg_di_stalled  <= di_stalled;
  o_dbg_ex_stalled  <= ex_stalled;
  o_dbg_wb_stalled  <= wb_stalled;

  o_dbg_jump_pc            <= wb_is_jump;
  o_dbg_jump_target        <= wb_jump_target;
  o_dbg_commited_instr_tag <= commited_instr_tag;

  o_dbg_wb2di_reg1 <= wb2di_reg1;
  o_dbg_wb2di_reg2 <= wb2di_reg2;

  o_dbg_if_instr_tag <= if_instr_tag;
  o_dbg_di_instr_tag <= di_instr_tag;
  o_dbg_ex_instr_tag <= ex_instr_tag;
  o_dbg_wb_instr_tag <= wb_instr_tag;

end architecture rtl;

-------------------------------------------------------------------------------
