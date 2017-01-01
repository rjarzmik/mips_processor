-------------------------------------------------------------------------------
-- Title      : Testbench for design "Instruction_Provider"
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : Instruction_Provider_tb.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-03
-- Last update: 2017-01-03
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-03  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.cache_defs.all;
use work.instruction_defs.all;

-------------------------------------------------------------------------------

entity Instruction_Provider_tb is

end entity Instruction_Provider_tb;

-------------------------------------------------------------------------------

architecture test of Instruction_Provider_tb is
  -- component generics
  constant ADDR_WIDTH : integer := 32;
  constant DATA_WIDTH : integer := 32;

  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  -- component ports
  signal clk          : std_logic := '1';
  signal rst          : std_logic := '1';
  signal o_pc         : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal o_data       : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_valid      : std_logic;
  signal o_do_step_pc : std_logic;
  -- L2 connections
  signal cls_creq     : cache_request_t;
  signal cls_cresp    : cache_response_t;

  signal o_L2c_req       : std_logic;
  signal o_L2c_we        : std_logic;
  signal o_L2c_addr      : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal i_L2c_read_data : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal o_L2c_wdata     : std_logic_vector(DATA_WIDTH - 1 downto 0);
  signal i_L2c_valid     : std_logic;

  signal next_pc    : addr_t;
  signal after_pc   : addr_t;
  signal next_itag  : instr_tag_t := INSTR_TAG_NONE;
  signal after_itag : instr_tag_t := INSTR_TAG_NONE;

begin  -- architecture test

  -- component instantiation
  DUT : entity work.Instruction_Provider
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk                      => clk,
      rst                      => rst,
      kill_req                 => '0',
      stall_req                => '0',
      i_next_pc                => next_pc,
      i_next_pc_instr_tag      => next_itag,
      i_next_next_pc           => after_pc,
      i_next_next_pc_instr_tag => after_itag,
      o_pc                     => o_pc,
      o_data                   => o_data,
      o_valid                  => o_valid,
      o_do_step_pc             => o_do_step_pc,
      o_creq                   => cls_creq,
      i_cresp                  => cls_cresp);

  -- reset
  rst <= '0'     after 12 ps;
  -- clock generation
  clk <= not clk after 5 ps;

  -- waveform generation
  WaveGen_Proc : process
  begin
    -- insert signal assignments here

    wait until Clk = '1';
  end process WaveGen_Proc;

  pc_emulator : process(clk, rst)
  begin
    if rst = '1' then
      next_pc  <= std_logic_vector(to_signed(0, ADDR_WIDTH));
      after_pc <= std_logic_vector(to_unsigned(4, ADDR_WIDTH));
    elsif rst = '0' and rising_edge(clk) then
      if o_do_step_pc = '1' then
        --if unsigned(next_pc) = to_unsigned(20, ADDR_WIDTH) then
        --next_pc  <= std_logic_vector(to_unsigned(8, ADDR_WIDTH));
        --after_pc <= std_logic_vector(to_unsigned(12, ADDR_WIDTH));
        if unsigned(next_pc) = to_unsigned(16, ADDR_WIDTH) then
          next_pc  <= std_logic_vector(to_unsigned(20, ADDR_WIDTH));
          after_pc <= std_logic_vector(to_unsigned(8, ADDR_WIDTH));
        else
          next_pc  <= after_pc;
          after_pc <= std_logic_vector(unsigned(after_pc) + 4);
        end if;
      end if;
    end if;
  end process pc_emulator;

  cls : entity work.cache_line_streamer
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      DATAS_PER_LINE_WIDTH => DATAS_PER_LINE_WIDTH)
    port map (
      clk     => clk,
      rst     => rst,
      i_creq  => cls_creq,
      o_cresp => cls_cresp,

      o_memory_req   => o_L2c_req,
      o_memory_we    => o_L2c_we,
      o_memory_addr  => o_L2c_addr,
      o_memory_wdata => o_L2c_wdata,
      i_memory_rdata => i_L2c_read_data,
      i_memory_done  => i_L2c_valid);

  Simulated_Memory_1 : entity work.Simulated_Memory
    generic map (
      ADDR_WIDTH     => ADDR_WIDTH,
      DATA_WIDTH     => DATA_WIDTH,
      MEMORY_LATENCY => 3)
    port map (
      clk                 => clk,
      rst                 => rst,
      i_memory_req        => o_L2c_req,
      i_memory_we         => o_L2c_we,
      i_memory_addr       => o_L2c_addr,
      i_memory_write_data => o_L2c_wdata,
      o_memory_read_data  => i_L2c_read_data,
      o_memory_valid      => i_L2c_valid);

end architecture test;

-------------------------------------------------------------------------------

configuration Instruction_Provider_tb_test_cfg of Instruction_Provider_tb is
  for test
  end for;
end Instruction_Provider_tb_test_cfg;

-------------------------------------------------------------------------------
