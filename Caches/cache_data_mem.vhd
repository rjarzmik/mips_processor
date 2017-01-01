-------------------------------------------------------------------------------
-- Title      : Cache content memory
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : cache_data_mem.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-17
-- Last update: 2016-12-28
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-17  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.cache_defs.all;

-------------------------------------------------------------------------------

entity cache_data_mem is
  generic (
    DEBUG : boolean := false
    );

  port (
    clk     : in  std_logic;
    -- Reader
    i_raddr : in  addr_t;
    i_re : in std_logic;
    i_rway   : in  natural range 0 to NB_WAYS - 1;
    --- o_rdata = o_data_by_way(i_rway)
    o_rdata : out data_t;

    -- Writer
    i_waddr : in addr_t;
    i_wway  : in way_selector_t; -- write way by way
    i_wdata : in data_t
    );

end entity cache_data_mem;

-------------------------------------------------------------------------------

architecture str of cache_data_mem is

  constant data_one_nb_bits  : natural := integer(ceil(log2(real(DATA_WIDTH / 8))));
  constant data_line_nb_bits : natural := DATAS_PER_LINE_WIDTH;
  constant data_set_nb_bits  : natural := data_one_nb_bits + data_line_nb_bits;
  constant index_nb_bits     : natural := INDEX_WIDTH;

  -- Entry anatomy, ie. entry for a given address index
  --- entry  [ [way_M-1_data] [way_M-2_data] ... [way_0_data] ]
  --- way_X_data = cached data
  subtype cache_entry_t is data_vector(0 to NB_WAYS - 1);

  -- Tag Status Memories interface
  subtype mem_addr_t is std_logic_vector(data_line_nb_bits + index_nb_bits - 1 downto 0);
  subtype mem_entry_t is std_logic_vector(NB_WAYS * DATA_WIDTH - 1 downto 0);
  signal mem_raddr      : mem_addr_t;
  signal mem_waddr      : mem_addr_t;
  signal mem_read_data  : cache_entry_t := (others => (others => '0'));
  signal mem_write_data : cache_entry_t := (others => (others => '0'));
  signal mem_rren       : way_selector_t := (others => '0');
  signal mem_wren       : way_selector_t := (others => '0');

  function get_mem_addr(i_addr : addr_t) return mem_addr_t is
    variable index        : natural range 0 to NB_LINES - 1;
    variable data_in_line : natural range 0 to DATAS_PER_LINE - 1;
  begin
    index     := get_address_index(i_addr);
    data_in_line := get_data_set_index(i_addr);
    return std_logic_vector(to_unsigned(index, INDEX_WIDTH)) &
      std_logic_vector(to_unsigned(data_in_line, DATAS_PER_LINE_WIDTH));
  end function get_mem_addr;

begin  -- architecture str

  cd : for i in 0 to NB_WAYS - 1 generate
    cdata : entity work.memory_cacheline_internal
      generic map (
        ADDR_WIDTH => index_nb_bits + data_line_nb_bits,
        DEBUG_IDX => i,
        DEBUG => DEBUG)
      port map (
        clock => clk,
        rren  => mem_rren(i),
        raddr => mem_raddr,
        waddr => mem_waddr,
        data  => mem_write_data(i),
        wren  => mem_wren(i),
        q     => mem_read_data(i));
  end generate cd;

  mem_raddr <= get_mem_addr(i_raddr);
  mem_waddr <= get_mem_addr(i_waddr);

  mem_rrens : for way in mem_rren'range generate
    mem_rren(way) <= i_re;
  end generate mem_rrens;

  mem_wren <= i_wway;

  o_rdata_mux : entity work.data_t_mux
    generic map (NB => NB_WAYS)
    port map (i_rway, mem_read_data, o_rdata);

  memwdatas : for way in mem_write_data'range generate
    mem_write_data(way) <= i_wdata;
  end generate;

end architecture str;

-------------------------------------------------------------------------------
