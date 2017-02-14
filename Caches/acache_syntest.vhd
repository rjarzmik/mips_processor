-------------------------------------------------------------------------------
-- Title      : Single input port associative cache synthesis test
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : acache_syntest.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-11-30
-- Last update: 2017-02-14
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Cache suited for an L1 cache
-------------------------------------------------------------------------------
-- Copyright (c) 2016
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-11-30  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.cache_defs.all;

entity acache_syntest is
  generic (
    ADDR_WIDTH       : positive := 26;
    DATA_WIDTH       : positive := 32;
    DATAS_PER_LINE   : positive := 16;
    NB_WAYS          : positive := 2;
    CACHE_SIZE_BYTES : positive := 8192;
    LOWER_DATA_WIDTH : positive := 64;
    WRITE_BACK       : boolean  := false;
    DEBUG            : boolean  := false
    );

  port (
    clk : in std_logic;
    rst : in std_logic;

    i_req              : in  std_logic;
    i_wen              : in  std_logic;
    i_addr             : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_wdata            : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
    i_do_write_through : in  std_logic;
    o_rdata            : out std_logic_vector(DATA_WIDTH - 1 downto 0);
    o_rdata_valid      : out std_logic;
    o_wready           : out std_logic;

    -- outer mem interface
    o_memory_req   : out std_logic;
    o_memory_we    : out std_logic;
    o_memory_addr  : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_memory_wdata : out std_logic_vector(LOWER_DATA_WIDTH - 1 downto 0);
    i_memory_rdata : in  std_logic_vector(LOWER_DATA_WIDTH - 1 downto 0);
    i_memory_done  : in  std_logic;

    o_dbg_cstats : out cache_stats_t
    );
end entity acache_syntest;

architecture str of acache_syntest is
  signal i_addr_r : std_logic_vector(ADDR_WIDTH - 1 downto 0);
  
begin
  acache_1 : entity work.acache
    generic map (ADDR_WIDTH       => ADDR_WIDTH,
                 DATA_WIDTH       => DATA_WIDTH,
                 DATAS_PER_LINE   => DATAS_PER_LINE,
                 NB_WAYS          => NB_WAYS,
                 CACHE_SIZE_BYTES => CACHE_SIZE_BYTES,
                 LOWER_DATA_WIDTH => LOWER_DATA_WIDTH,
                 WRITE_BACK       => WRITE_BACK,
                 DEBUG            => DEBUG)
    port map (
      clk                => clk,
      rst                => rst,
      i_req              => i_req,
      i_wen              => i_wen,
      i_addr             => i_addr_r,
      i_wdata            => i_wdata,
      i_do_write_through => i_do_write_through,
      o_rdata            => o_rdata,
      o_rdata_valid      => o_rdata_valid,
      o_wready           => o_wready,
      o_memory_req       => o_memory_req,
      o_memory_we        => o_memory_we,
      o_memory_addr      => o_memory_addr,
      o_memory_wdata     => o_memory_wdata,
      i_memory_rdata     => i_memory_rdata,
      i_memory_done      => i_memory_done,
      o_dbg_cstats       => o_dbg_cstats);

  process(clk, i_addr)
  begin
    if rising_edge(clk) then
      i_addr_r <= i_addr;
    end if;
  end process;

end architecture str;
