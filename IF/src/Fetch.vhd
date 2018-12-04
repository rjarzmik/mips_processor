-------------------------------------------------------------------------------
-- Title      : Instruction Fetch stage
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : Fetch.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-11-10
-- Last update: 2018-12-04
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Fetch instruction from I-Cache and forward to Decode-Issue
-------------------------------------------------------------------------------
-- Copyright (c) 2016 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-11-10  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.cache_defs.all;

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

    i_pc     : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_pc_req : out std_logic;

    o_pc_instr                  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_instruction               : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_instr_tag                 : out instr_tag_t;

    -- L2 connections
    o_l2c_req                   : out std_logic;
    o_l2c_we                    : out std_logic;
    o_l2c_addr                  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_l2c_rdata                 : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_l2c_wdata                 : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_l2c_done                  : in  std_logic;

    -- Debug signals
    o_dbg_if_pc                 : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_if_fetching_pc        : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_dbg_if_fetching_instr_tag : out instr_tag_t;
    o_dbg_prediction            : out prediction_t
    );

end entity Fetch;

architecture instr_is_pc of Fetch is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  constant nop_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
begin

  fake_fetch : process(rst, clk)
    variable pc : unsigned(ADDR_WIDTH - 1 downto 0) := (others => '0');
  begin
    if rst = '1' then
      pc            := to_unsigned(0, pc'length);
      o_instruction <= std_logic_vector(pc);
      o_pc_instr    <= std_logic_vector(pc);
    elsif rising_edge(clk) then
      if kill_req = '1' then
        o_instruction     <= (others => '0');
      elsif stall_req then
      else
        pc                := pc + 4;
        o_instruction     <= std_logic_vector(pc);
        o_pc_instr        <= std_logic_vector(pc);
      end if;
    end if;
  end process fake_fetch;
end architecture instr_is_pc;

architecture simple of Fetch is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  constant nop_instruction : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');

  -- Instruction killing
  signal r_kill_req      : std_logic;
  signal kill_next_valid : boolean;

  -- Cache address queries
  signal query_pc          : addr_t;
  signal next_query_pc_req : std_logic;
  signal next_query_pc     : addr_t;

  -- Currently fetching
  signal fetching_pc    : addr_t;
  signal fetching_data  : data_t;
  signal fetching_valid : std_logic;

  -- Fetched instruction
  signal fetched_pc   : addr_t;
  signal fetched_data : data_t;
  signal itag         : instr_tag_t;

  -- PC Predictor
  signal predict_current : addr_t;
  signal predict_next    : addr_t;

  -- Outgoing to next pipeline stage instruction
  signal out_pc     : addr_t;
  signal out_data   : data_t;
  signal data_valid : std_logic;
begin
  iprovider : entity work.Instruction_Provider
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk             => clk,
      rst             => rst,
      kill_req        => kill_req,
      stall_req       => stall_req,
      i_next_addr     => next_query_pc,
      o_addr          => fetching_pc,
      o_data          => fetching_data,
      o_valid         => fetching_valid,
      o_next_addr_req => next_query_pc_req,
      o_l2c_req       => o_l2c_req,
      o_l2c_we        => o_l2c_we,
      o_l2c_addr      => o_l2c_addr,
      i_l2c_rdata     => i_l2c_rdata,
      o_l2c_wdata     => o_l2c_wdata,
      i_l2c_done      => i_l2c_done);

  jumper : process(rst, clk, kill_req, fetching_valid)
  begin
    if rst = '1' then
      kill_next_valid <= false;
    elsif rising_edge(clk) and kill_req = '1' then
      kill_next_valid <= true;
    elsif rising_edge(clk) and fetching_valid = '1' then
      kill_next_valid <= false;
    end if;
  end process jumper;

  provider_driver : process(rst, clk, next_query_pc_req, query_pc)
  begin
    --
    -- RJK: next_query_pc should be retrieved out of predict_next
    --

    if rst = '1' then
      next_query_pc <= std_logic_vector(to_unsigned(STEP, next_query_pc'length));
    else
      next_query_pc <= std_logic_vector(unsigned(query_pc) + STEP);
    end if;

    if rst = '1' then
      query_pc <= std_logic_vector(to_unsigned(STEP, next_query_pc'length));
    elsif rising_edge(clk) then
      if next_query_pc_req = '1' then
        query_pc        <= next_query_pc;
        predict_current <= next_query_pc;
      end if;
    end if;
  end process provider_driver;

  cache_aquire : process(rst, clk, stall_req, kill_req, r_kill_req, fetching_valid)
  begin
    if rst = '1' then
    elsif rising_edge(clk) then
      r_kill_req <= kill_req;

      if fetching_valid = '1' and rst = '0' and
        kill_req = '0' and r_kill_req = '0' and not kill_next_valid and
        stall_req = '0' then
        fetched_pc   <= fetching_pc;
        fetched_data <= fetching_data;
      elsif stall_req = '0' then
        fetched_pc   <= (others => 'X');
        fetched_data <= nop_instruction;
      end if;
    end if;
  end process cache_aquire;

  out_pc   <= fetched_pc;
  out_data <= fetched_data;

  o_pc_req      <= next_query_pc_req;
  o_pc_instr    <= out_pc;
  o_instruction <= out_data;

  o_dbg_if_pc                 <= fetched_pc;
  o_dbg_if_fetching_pc        <= fetching_pc;
  o_dbg_if_fetching_instr_tag <= itag;

end architecture simple;
