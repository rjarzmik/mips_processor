-------------------------------------------------------------------------------
-- Title      : Instructions Provider
-- Project    : MIPS Processor
-------------------------------------------------------------------------------
-- File       : Instruction_Provider.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-12-03
-- Last update: 2018-12-04
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Provides a flow of instructions to the processor
--   Working model :
--     - if kill_req = '1' :
--       - all currently fetching instructions are dropped
--       - the next to query instruction is programmed at @i_next_addr
--     - if stall_req = '1' :
--       - the address pushed on the cache query is stalled
--       - even if the value on o_data is valid, o_valid is forced to '0'
--     - else
--       - when o_next_addr_req = '1', in the same cycle, the value on
--         i_next_addr will be latched on the cache next address
--       - when o_valid = '1', the data read is provided on the couple
--         (o_addr, o_data).
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-12-03  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cpu_defs.all;
use work.cache_defs.all;

entity Instruction_Provider is
  generic (
    ADDR_WIDTH : integer;
    DATA_WIDTH : integer
    );

  port (
    clk             : in  std_logic;
    rst             : in  std_logic;
    -- control
    --- kill_req = 1 implies that the currently fetched data should be killed,
    --- ie. o_valid sould be '0' for them, and the address in i_addr should be
    --- the one to fetch next.
    kill_req        : in  std_logic;
    --- stall_req = 1 implies nothing is latched, and the cache query
    --- doesn't go forward
    stall_req       : in  std_logic;
    -- addresses where to fetch from
    i_next_addr     : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    -- fetched data
    o_addr          : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_data          : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_valid         : out std_logic;
    -- address request : request a new address in i_next_addr
    o_next_addr_req : out std_logic;
    -- L2 connections
    o_l2c_req       : out std_logic;
    o_l2c_we        : out std_logic;
    o_l2c_addr      : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_l2c_rdata     : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_l2c_wdata     : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_l2c_done      : in  std_logic
    );
end entity Instruction_Provider;

architecture str of Instruction_Provider is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  -- Cycles of addresses
  --   query_pc    ->    fetching_pc             -> fetched_pc
  --   (on cache I/F)    wait for fetching_valid -> output for IF

  -- Cache address queries
  signal first_query   : boolean := true;
  signal change_query  : boolean;
  signal query_pc      : addr_t;
  signal next_query_pc : addr_t;

  -- Currently fetching
  signal fetching_pc    : addr_t;
  signal fetching_data  : data_t;
  signal fetching_valid : std_logic;
begin
  L1C : entity work.Instruction_Cache
    generic map (
      ADDR_WIDTH => ADDR_WIDTH,
      DATA_WIDTH => DATA_WIDTH)
    port map (
      clk         => clk,
      rst         => rst,
      addr        => query_pc,
      data        => fetching_data,
      data_valid  => fetching_valid,
      o_l2c_req   => o_l2c_req,
      o_l2c_addr  => o_l2c_addr,
      i_l2c_rdata => i_l2c_rdata,
      i_l2c_done  => i_l2c_done);

  cache_driver : process(rst, clk, stall_req, kill_req, i_next_addr,
                         fetching_valid, change_query, first_query, query_pc,
                         next_query_pc)
  begin
    next_query_pc <= i_next_addr;

    change_query <= (first_query or fetching_valid = '1' or kill_req = '1')
                    and stall_req = '0';

    if rst = '1' then
      first_query <= true;
    elsif first_query and rising_edge(clk) then
      first_query <= false;
    end if;

    if rst = '1' then
      query_pc        <= (others => '0');
      fetching_pc     <= (others => '0');
    elsif rising_edge(clk) and change_query then
      fetching_pc <= query_pc;
      query_pc    <= next_query_pc;
    end if;
  end process cache_driver;

  o_l2c_we        <= '0';
  o_l2c_wdata     <= (others => 'X');
  o_next_addr_req <= '1' when change_query else '0';

  -- Outputs
  o_addr  <= fetching_pc;
  o_data  <= fetching_data;
  o_valid <= fetching_valid;
end architecture str;
