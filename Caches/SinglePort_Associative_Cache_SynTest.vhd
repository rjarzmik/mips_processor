-------------------------------------------------------------------------------
-- Title      : Synthesis test for single ported associative cache
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : SinglePort_Associative_Cache_SynTest.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-27
-- Last update: 2016-12-30
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-27  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.all;

-------------------------------------------------------------------------------

entity SinglePort_Associative_Cache_SynTest is

  generic (
    WRITE_BACK : boolean := true
    );

  port (
    clk : in std_logic;
    rst : in std_logic;

    i_porta_req              : in  std_logic;
    i_porta_we               : in  std_logic;
    i_porta_addr             : in  std_logic_vector(ADDR_WIDTH - 1 downto ADDR_DATA_NBITS);
    i_porta_do_write_through : in  std_logic;
    i_porta_write_data       : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_porta_read_data        : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_porta_valid            : out std_logic;
    o_porta_rdy              : out std_logic;

    -- outer mem interface
    o_memory_req   : out std_logic;
    o_memory_we    : out std_logic;
    o_memory_addr  : out std_logic_vector(ADDR_WIDTH - 1 downto 2);
    i_memory_rdata : in  data_t;
    o_memory_wdata : out data_t;
    i_memory_done  : in  std_logic

    );

end entity SinglePort_Associative_Cache_SynTest;

-------------------------------------------------------------------------------

architecture str of SinglePort_Associative_Cache_SynTest is

  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal creq  : cache_request_t;
  signal cresp : cache_response_t;

  signal porta_addr : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  signal memory_addr : std_logic_vector(ADDR_WIDTH - 1 downto 0);

begin  -- architecture str

  porta_addr <= i_porta_addr & std_logic_vector(to_unsigned(0, ADDR_DATA_NBITS));
  o_memory_addr <= memory_addr(ADDR_WIDTH - 1 downto ADDR_DATA_NBITS);

  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------
  SinglePort_Associative_Cache_1 : entity work.SinglePort_Associative_Cache
    generic map (
      WRITE_BACK => WRITE_BACK)
    port map (
      clk                      => clk,
      rst                      => rst,
      i_porta_req              => i_porta_req,
      i_porta_we               => i_porta_we,
      i_porta_addr             => porta_addr,
      i_porta_do_write_through => i_porta_do_write_through,
      i_porta_write_data       => i_porta_write_data,
      o_porta_read_data        => o_porta_read_data,
      o_porta_valid            => o_porta_valid,
      o_porta_rdy              => o_porta_rdy,
      o_creq                   => creq,
      i_cresp                  => cresp);

  DUT : entity work.cache_line_streamer
    generic map (
      ADDR_WIDTH           => ADDR_WIDTH,
      DATA_WIDTH           => DATA_WIDTH,
      DATAS_PER_LINE_WIDTH => DATAS_PER_LINE_WIDTH)
    port map (
      clk     => clk,
      rst     => rst,
      i_creq  => creq,
      o_cresp => cresp,

      o_memory_req   => o_memory_req,
      o_memory_we    => o_memory_we,
      o_memory_addr  => memory_addr,
      i_memory_rdata => i_memory_rdata,
      o_memory_wdata => o_memory_wdata,
      i_memory_done  => i_memory_done);

end architecture str;

-------------------------------------------------------------------------------
