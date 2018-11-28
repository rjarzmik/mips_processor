-------------------------------------------------------------------------------
-- Title      : Branch Target Buffer Synthesis Test
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : branch_target_buffer_syntest.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-25
-- Last update: 2018-11-28
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2017  Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-02-25  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity branch_target_buffer_syntest is
  generic (
    ADDR_WIDTH       : natural  := 30;
    NB_WAYS          : positive := 2;
    CACHE_SIZE_BYTES : positive := 1024;
    DEBUG            : boolean  := false
    );

  port (
    clk        : in  std_logic;
    stall      : in  std_logic;
    query_addr : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    reply_addr : out std_logic_vector(ADDR_WIDTH - 1 downto 0);
    reply_ways : out std_logic_vector(0 to NB_WAYS - 1);
    --
    update     : in  std_logic;
    wsrc_addr  : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    wtgt_addr  : in  std_logic_vector(ADDR_WIDTH - 1 downto 0)
    );

end entity branch_target_buffer_syntest;

architecture str of branch_target_buffer_syntest is
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);

  signal r_query_addr : addr_t;
  signal r_wsrc_addr  : addr_t;
  signal r_wtgt_addr  : addr_t;
  signal o_reply_addr : addr_t;
  signal o_reply_ways : std_logic_vector(0 to NB_WAYS - 1);
  signal r_update     : std_logic;
begin  -- architecture str
  branch_target_buffer_1 : entity work.branch_target_buffer
    generic map (
      ADDR_WIDTH        => ADDR_WIDTH,
      NB_WAYS           => NB_WAYS,
      CACHE_SIZE_BYTES  => CACHE_SIZE_BYTES,
      TWO_CYCLES_ANSWER => true,
      DEBUG             => DEBUG)
    port map (
      clk        => clk,
      stall      => stall,
      query_addr => r_query_addr,
      reply_addr => o_reply_addr,
      reply_ways => o_reply_ways,
      update     => r_update,
      wsrc_addr  => r_wsrc_addr,
      wtgt_addr  => r_wtgt_addr);

  process(clk)
  begin
    if rising_edge(clk) then
      r_query_addr <= query_addr;
      r_update     <= update;
      r_wsrc_addr  <= wsrc_addr;
      r_wtgt_addr  <= wtgt_addr;
      reply_addr   <= o_reply_addr;
      reply_ways   <= o_reply_ways;
    end if;
  end process;
end architecture str;
