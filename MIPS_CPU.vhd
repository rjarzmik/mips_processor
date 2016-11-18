-------------------------------------------------------------------------------
-- Title      : MIPS Processor
-- Project    : 
-------------------------------------------------------------------------------
-- File       : MIPS_CPU.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-11
-- Last update: 2016-11-18
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
    clk : in std_logic;
    rst : in std_logic
    );

end entity MIPS_CPU;

-------------------------------------------------------------------------------

architecture rtl of MIPS_CPU is

  component PC_Register is
    generic (
      ADDR_WIDTH : integer;
      STEP       : integer);
    port (
      clk         : in  std_logic;
      rst         : in  std_logic;
      stall_pc    : in  std_logic;
      jump_pc     : in  std_logic;
      jump_target : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      current_pc  : out std_logic_vector(ADDR_WIDTH - 1 downto 0));
  end component PC_Register;

  component Fetch
    port (
      clk         : in  std_logic;
      rst         : in  std_logic;
      stall_req   : in  std_logic;
      pc          : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      instruction : out std_logic_vector(DATA_WIDTH - 1 downto 0)
      );
  end component Fetch;

  component Decode is
    generic (
      ADDR_WIDTH           : integer;
      DATA_WIDTH           : integer;
      NB_REGISTERS         : positive;
      NB_REGISTERS_SPECIAL : positive;
      REG_IDX_MFLO         : natural;
      REG_IDX_MFHI         : natural);
    port (
      clk         : in  std_logic;
      rst         : in  std_logic;
      stall_req   : in  std_logic;
      instruction : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      pc          : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      i_rwb_reg1  : in  register_port_type;
      i_rwb_reg2  : in  register_port_type;
      alu_op      : out alu_op_type;
      o_reg1      : out register_port_type;
      o_reg2      : out register_port_type;
      jump_target : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      jump_op     : out jump_type;
      mem_data    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
      mem_op      : out memory_op_type);
  end component Decode;

  component ALU is
    generic (
      ADDR_WIDTH   : integer;
      DATA_WIDTH   : integer;
      NB_REGISTERS : positive);
    port (
      clk           : in  std_logic;
      rst           : in  std_logic;
      stall_req     : in  std_logic;
      alu_op        : in  alu_op_type;
      i_reg1        : in  register_port_type;
      i_reg2        : in  register_port_type;
      i_jump_target : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      i_jump_op     : in  jump_type;
      i_mem_data    : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
      i_mem_op      : in  memory_op_type;
      o_reg1        : out register_port_type;
      o_reg2        : out register_port_type;
      o_jump_target : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
      o_is_jump     : out std_logic;
      o_mem_data    : out std_logic_vector(DATA_WIDTH - 1 downto 0);
      o_mem_op      : out memory_op_type);
  end component ALU;

  component Writeback is
    generic (
      ADDR_WIDTH   : integer;
      DATA_WIDTH   : integer;
      NB_REGISTERS : positive);
    port (
      clk           : in  std_logic;
      rst           : in  std_logic;
      stall_req     : in  std_logic;
      i_reg1        : in  register_port_type;
      i_reg2        : in  register_port_type;
      i_jump_target : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
      i_is_jump     : in  std_logic;
      o_reg1        : out register_port_type;
      o_reg2        : out register_port_type;
      o_is_jump     : out std_logic;
      o_jump_target : out std_logic_vector(ADDR_WIDTH - 1 downto 0));
  end component Writeback;

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal current_pc          : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal jump_target         : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal fetched_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal alu_op              : alu_op_type;
  signal di2ex_ra            : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal di2ex_rb            : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal di2ex_reg1          : register_port_type;
  signal di2ex_reg1_we       : std_logic;
  signal di2ex_reg1_idx      : natural range 0 to NB_REGISTERS_GP + NB_REGISTERS_SPECIAL - 1;
  signal di2ex_reg2          : register_port_type;
  signal di2ex_reg2_we       : std_logic;
  signal di2ex_reg2_idx      : natural range 0 to NB_REGISTERS_GP + NB_REGISTERS_SPECIAL - 1;
  signal stall_pc            : std_logic;
  signal jump_pc             : std_logic;
  signal wb2di_reg1          : register_port_type;
  signal wb2di_reg2          : register_port_type;
  signal di2ex_jump_target   : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal di2ex_jump_op       : jump_type;
  signal di2ex_mem_data      : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal di2ex_mem_op        : memory_op_type;

  signal ex2wb_reg1        : register_port_type;
  signal ex2wb_reg2        : register_port_type;
  signal ex2wb_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal ex2wb_is_jump     : std_logic;
  signal ex2wb_mem_data    : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal ex2wb_mem_op      : memory_op_type;

  signal wb_is_jump     : std_logic;
  signal wb_jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  pc_reg : PC_Register
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => 4)
    port map (
      clk         => clk,
      rst         => rst,
      stall_pc    => stall_pc,
      jump_pc     => wb_is_jump,
      jump_target => wb_jump_target,
      current_pc  => current_pc);

  ife : Fetch
    port map (
      clk         => clk,
      rst         => rst,
      stall_req   => '0',
      pc          => current_pc,
      instruction => fetched_instruction
      );

  di : Decode
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      NB_REGISTERS         => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL,
      NB_REGISTERS_SPECIAL => NB_REGISTERS_SPECIAL,
      REG_IDX_MFLO         => 32,
      REG_IDX_MFHI         => 33)
    port map (
      clk         => clk,
      rst         => rst,
      stall_req   => '0',
      instruction => fetched_instruction,
      pc          => current_pc,
      i_rwb_reg1  => wb2di_reg1,
      i_rwb_reg2  => wb2di_reg2,
      alu_op      => alu_op,
      o_reg1      => di2ex_reg1,
      o_reg2      => di2ex_reg2,
      jump_target => di2ex_jump_target,
      jump_op     => di2ex_jump_op,
      mem_data    => di2ex_mem_data,
      mem_op      => di2ex_mem_op);


  ex : ALU
    generic map (
      ADDR_WIDTH   => ADDR_WIDTH,
      DATA_WIDTH   => DATA_WIDTH,
      NB_REGISTERS => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL)
    port map (
      clk           => clk,
      rst           => rst,
      stall_req     => '0',
      alu_op        => alu_op,
      i_reg1        => di2ex_reg1,
      i_reg2        => di2ex_reg2,
      i_jump_target => di2ex_jump_target,
      i_jump_op     => di2ex_jump_op,
      i_mem_data    => di2ex_mem_data,
      i_mem_op      => di2ex_mem_op,
      o_reg1        => ex2wb_reg1,
      o_reg2        => ex2wb_reg2,
      o_jump_target => ex2wb_jump_target,
      o_is_jump     => ex2wb_is_jump,
      o_mem_data    => ex2wb_mem_data,
      o_mem_op      => ex2wb_mem_op);

  wb : Writeback
    generic map (
      ADDR_WIDTH   => ADDR_WIDTH,
      DATA_WIDTH   => DATA_WIDTH,
      NB_REGISTERS => NB_REGISTERS_GP + NB_REGISTERS_SPECIAL)
    port map (
      clk           => clk,
      rst           => rst,
      stall_req     => '0',
      i_reg1        => ex2wb_reg1,
      i_reg2        => ex2wb_reg2,
      i_jump_target => ex2wb_jump_target,
      i_is_jump     => ex2wb_is_jump,
      o_reg1        => wb2di_reg1,
      o_reg2        => wb2di_reg2,
      o_is_jump     => wb_is_jump,
      o_jump_target => wb_jump_target);

  stall_pc <= '0';

end architecture rtl;

-------------------------------------------------------------------------------
