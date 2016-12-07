-------------------------------------------------------------------------------
-- Title      : Instruction Fetch stage
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Fetch.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-10
-- Last update: 2016-12-07
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Fetch instruction from I-Cache and forward to Decode-Issue
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-11-10  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
-------------------------------------------------------------------------------

entity Fetch is

  generic (
    ADDR_WIDTH : integer := 32;
    DATA_WIDTH : integer := 32;
    STEP       : natural := 4
    );

  port (
    clk       : in std_logic;
    rst       : in std_logic;
    stall_req : in std_logic;           -- stall current instruction
    kill_req  : in std_logic;           -- kill current instruction

    o_pc_instr           : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_instruction        : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_instr_tag          : out instr_tag_t;
    -- L2 connections
    o_L2c_req            : out std_logic;
    o_L2c_addr           : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_L2c_read_data      : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_L2c_valid          : in  std_logic;
    -- Writeback feedback signals
    i_is_jump            : in  std_logic;
    i_jump_target        :     std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- Debug signals
    o_dbg_if_pc          : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_if_fetching_pc : out std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity Fetch;

-------------------------------------------------------------------------------

architecture rtl3 of Fetch is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  constant nop_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');

  --- Control signal
  signal kill_next_pc : std_logic;
  signal do_stall_pc  : std_logic;

  --- Signal from program counter provider
  signal pcprovider_pc      : addr_t;
  signal pcprovider_next_pc : addr_t;

  --- Signals from instruction provider
  signal iprovider_pc           : addr_t;
  signal iprovider_data         : data_t;
  signal iprovider_data_valid   : std_logic;
  signal iprovider_do_step_pc   : std_logic;
  signal dbg_iprovider_fetching : addr_t;

  --- Outgoing to next pipeline stage instruction
  signal out_pc        : addr_t;
  signal out_data      : data_t;
  signal out_instr_tag : instr_tag_t;

begin
  iprovider : entity work.Instruction_Provider
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk             => clk,
      rst             => rst,
      kill_req        => kill_next_pc,
      stall_req       => stall_req,
      i_next_pc       => pcprovider_pc,
      i_next_next_pc  => pcprovider_next_pc,
      o_pc            => iprovider_pc,
      o_data          => iprovider_data,
      o_valid         => iprovider_data_valid,
      o_do_step_pc    => iprovider_do_step_pc,
      o_L2c_req       => o_L2c_req,
      o_L2c_addr      => o_L2c_addr,
      i_L2c_read_data => i_L2c_read_data,
      i_L2c_valid     => i_L2c_valid,
      o_dbg_fetching  => dbg_iprovider_fetching);

  pc_reg : entity work.PC_Register
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      STEP       => STEP)
    port map (
      clk          => clk,
      rst          => rst,
      stall_pc     => do_stall_pc,
      jump_pc      => i_is_jump,
      jump_target  => i_jump_target,
      o_current_pc => pcprovider_pc,
      o_next_pc    => pcprovider_next_pc);

  --- PC stepper
  do_stall_pc <= '1' when iprovider_do_step_pc = '0' else '0';

  --- PC jump handler
  kill_next_pc <= kill_req;

  --- Decode input provider
  o_instruction <= out_data;
  o_pc_instr    <= out_pc;

  fetch_outputs_latcher : process(clk, rst, kill_req, stall_req)
  begin
    if rst = '1' then
      out_pc        <= (others => 'X');
      out_data      <= (others => 'X');
      out_instr_tag <= INSTR_TAG_NONE + 1;
      o_instr_tag   <= INSTR_TAG_NONE;
    elsif rising_edge(clk) then
      if kill_req = '1' then
        out_pc      <= (others => 'X');
        out_data    <= nop_instruction;
        o_instr_tag <= INSTR_TAG_NONE;
      elsif stall_req = '1' then
      else
        if iprovider_data_valid = '1' then
          out_pc   <= iprovider_pc;
          out_data <= iprovider_data;
          if ((out_instr_tag + 1) mod NB_PIPELINE_STAGES) = INSTR_TAG_NONE then
            out_instr_tag <= INSTR_TAG_NONE + 1;
          else
            out_instr_tag <= (out_instr_tag + 1) mod NB_PIPELINE_STAGES;
          end if;
          o_instr_tag <= out_instr_tag;
        else
          out_pc      <= (others => 'X');
          out_data    <= nop_instruction;
          o_instr_tag <= INSTR_TAG_NONE;
        end if;
      end if;
    end if;
  end process fetch_outputs_latcher;

  --- Debug signals
  o_dbg_if_pc          <= out_pc;
  o_dbg_if_fetching_pc <= dbg_iprovider_fetching;

end architecture rtl3;
