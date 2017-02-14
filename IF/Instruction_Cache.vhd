-------------------------------------------------------------------------------
-- Title      : Instruction Cache L1
-- Project    : 
-------------------------------------------------------------------------------
-- File       : Instruction_Cache.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-11-10
-- Last update: 2017-02-15
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Level 1 cache for instruction fetch
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

use work.cache_defs.all;

entity Instruction_Cache is

  generic (
    ADDR_WIDTH : integer;
    DATA_WIDTH : integer
    );

  port (
    clk         : in  std_logic;
    rst         : in  std_logic;
    -- stall_req       : in  std_logic;
    addr        : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    data        : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    data_valid  : out std_logic;
    -- L2 connections
    o_l2c_req   : out std_logic;
    o_l2c_we    : out std_logic;
    o_l2c_addr  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_l2c_rdata : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_l2c_wdata : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_l2c_done  : in  std_logic
    );

end entity Instruction_Cache;

architecture rtl of Instruction_Cache is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal i_porta_req         : std_logic;
  signal fetch_addr          : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal fetched_instr_valid : std_logic;
  signal first_instr         : std_ulogic;

begin  -- architecture rtl

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  L1_Instr : entity work.acache
    generic map (
      ADDR_WIDTH       => ADDR_WIDTH,
      DATA_WIDTH       => DATA_WIDTH,
      DATAS_PER_LINE   => 64,
      NB_WAYS          => 2,
      CACHE_SIZE_BYTES => 8192,
      LOWER_DATA_WIDTH => DATA_WIDTH,
      WRITE_BACK       => false,
      STATISTICS       => true,
      DEBUG            => false)
    port map (
      clk                => clk,
      rst                => rst,
      i_req              => i_porta_req,
      i_wen              => '0',
      i_addr             => fetch_addr,
      i_wdata            => (others => 'X'),
      i_do_write_through => '0',
      o_rdata            => data,
      o_rdata_valid      => fetched_instr_valid,
      -- Carry over
      o_memory_req       => o_l2c_req,
      o_memory_we        => o_l2c_we,
      o_memory_addr      => o_l2c_addr,
      i_memory_rdata     => i_l2c_rdata,
      i_memory_done      => i_l2c_done);

  i_porta_req <= fetched_instr_valid or first_instr;
  data_valid  <= fetched_instr_valid;
  fetch_addr  <= addr;

  first_instructor : process(rst, clk, first_instr)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        first_instr <= '1';
      else
        first_instr <= '0';
      end if;
    end if;
  end process first_instructor;

end architecture rtl;
