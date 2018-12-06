-------------------------------------------------------------------------------
-- Title      : Testbench for design "Fetch"
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Fetch_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-11
-- Last update: 2018-12-05
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
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

library rjarzmik;
use rjarzmik.Simulated_Memory;
-------------------------------------------------------------------------------

entity fetch_tb is

end entity fetch_tb;

-------------------------------------------------------------------------------

architecture rtl of fetch_tb is

  -- component generics
  constant ADDR_WIDTH     : integer := 16;
  constant DATA_WIDTH     : integer := 16;
  constant MEMORY_LATENCY : integer := 3;

  -- component ports

  -- clock
  signal Clk : std_logic := '1';
  -- reset
  signal Rst : std_logic := '1';

  signal instruction : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal pc          : std_logic_vector(ADDR_WIDTH - 1 downto 0) := std_logic_vector(to_unsigned(0, ADDR_WIDTH));
  signal next_pc     : std_logic_vector(ADDR_WIDTH - 1 downto 0) := std_logic_vector(to_unsigned(4, ADDR_WIDTH));
  signal stall_req   : std_logic                                 := '0';
  signal pc_req      : std_logic                                 := '0';
  signal kill_req    : std_logic                                 := '0';

  signal jump_pc     : std_logic;
  signal jump_target : std_logic_vector(ADDR_WIDTH - 1 downto 0);

  -- L2 connections
  signal o_L2c_req       : std_logic;
  signal o_L2c_addr      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal i_L2c_read_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_L2c_valid     : std_logic;

  -- Debug signals
  signal dbg_if_pc : std_logic_vector(ADDR_WIDTH - 1 downto 0);

begin  -- architecture rtl

  -- component instantiation
  dut : entity work.Fetch(simple)
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk                  => Clk,
      rst                  => Rst,
      stall_req            => '0',
      kill_req             => kill_req,
      i_pc                 => pc,
      o_pc_req             => pc_req,
      o_instruction        => instruction,
      o_l2c_req            => o_L2c_req,
      o_l2c_addr           => o_L2c_addr,
      i_l2c_rdata          => i_L2c_read_data,
      i_l2c_done           => i_l2c_valid,
      o_dbg_if_fetching_pc => dbg_if_pc);

  Simulated_Memory_1 : entity rjarzmik.Simulated_Memory
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      DATA_WIDTH        => DATA_WIDTH,
      MEMORY_ADDR_WIDTH => 16,
      MEMORY_LATENCY    => MEMORY_LATENCY,
      DEBUG             => true)
    port map (
      clk                 => Clk,
      rst                 => Rst,
      i_memory_req        => o_L2c_req,
      i_memory_we         => '0',
      i_memory_addr       => o_L2c_addr,
      i_memory_write_data => (others => 'X'),
      o_memory_read_data  => i_L2c_read_data,
      o_memory_valid      => i_L2c_valid);

  PC_Register_1 : entity work.PC_Register(simple)
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => 4)
    port map (
      clk          => Clk,
      rst          => Rst,
      stall_pc     => stall_req,
      jump_pc      => jump_pc,
      jump_target  => jump_target,
      o_current_pc => pc);

  -- reset
  Rst <= '0'     after 12 ps;
  -- clock generation
  Clk <= not Clk after 5 ps;

  --
  stall_req <= not pc_req;

  -- waveform generation
  WaveGen_Proc : process
    variable nb_clks : integer := 0;
  begin
    -- insert signal assignments here

    wait until Clk = '1';
    nb_clks := nb_clks + 1;

    if unsigned(pc) = to_unsigned(16 + 4, ADDR_WIDTH) then
      jump_pc     <= '1';
      kill_req    <= '1';
      jump_target <= std_logic_vector(to_unsigned(8, ADDR_WIDTH));
    else
      jump_pc     <= '0';
      kill_req    <= '0';
      jump_target <= (others => 'X');
    end if;

  end process WaveGen_Proc;

end architecture rtl;

-------------------------------------------------------------------------------

configuration fetch_tb_test_cfg of fetch_tb is
  for rtl
  end for;
end fetch_tb_test_cfg;

-------------------------------------------------------------------------------