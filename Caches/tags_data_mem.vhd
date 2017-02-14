-------------------------------------------------------------------------------
-- Title      : Cache tags data memory
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : tags_data_mem.vhd
-- Author     : Robert Jarzmik <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-12-15
-- Last update: 2017-02-13
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description:
-------------------------------------------------------------------------------
-- Copyright (c) 2016
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-12-15  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library rjarzmik;
use rjarzmik.memories.sc_sram;
use rjarzmik.slv_utils.or_reduce;
use rjarzmik.slv_utils.slv_2d;
use rjarzmik.slv_utils.slv_2d_mux;
use rjarzmik.slv_utils.assign_row;
use rjarzmik.slv_utils.slv_is_x;

use work.cache_defs.all;
use work.cache_sizing.all;

entity tags_data_mem is
  generic
    (
      ADDR_WIDTH       : positive;
      DATA_WIDTH       : positive;
      DATAS_PER_LINE   : positive;
      NB_WAYS          : positive;
      CACHE_SIZE_BYTES : positive;
      DEBUG            : boolean := false
      );

  port (
    clk             : in  std_logic;
    -- Reader
    i_re            : in  std_logic;
    i_raddr         : in  std_logic_vector(ADDR_WIDTH - 1 downto 0);
    o_found         : out boolean;
    o_way_found     : out natural range 0 to NB_WAYS - 1;
    o_rway          : out std_logic_vector(0 to NB_WAYS - 1);
    --- Combinational : read_tag_entries(i_raddr)(o_way_found)
    o_tag_slv       : out std_logic_vector(
      get_tag_slv_width(ADDR_WIDTH, DATA_WIDTH, DATAS_PER_LINE, NB_WAYS, CACHE_SIZE_BYTES) - 1 downto 0);
    o_way_evict     : out natural range 0 to NB_WAYS - 1;
    o_evict_dirty   : out boolean;
    o_evict_tag_slv : out std_logic_vector(
      get_tag_slv_width(ADDR_WIDTH, DATA_WIDTH, DATAS_PER_LINE, NB_WAYS, CACHE_SIZE_BYTES) - 1 downto 0);

    -- Writer
    i_waddr         : in std_logic_vector(ADDR_WIDTH - 1 downto 0);
    i_wway          : in std_logic_vector(0 to NB_WAYS - 1);
    i_wtag_slv      : in std_logic_vector(
      get_tag_slv_width(ADDR_WIDTH, DATA_WIDTH, DATAS_PER_LINE, NB_WAYS, CACHE_SIZE_BYTES) - 1 downto 0);
    --- i_way_alloc triggers a new election of the next to evict line
    i_evict_compute : in std_logic
    );
end entity tags_data_mem;

architecture str of tags_data_mem is
  -- Entry anatomy, ie. entry for a given address index
  constant cs : csizes := to_cache_sizing(ADDR_WIDTH, DATA_WIDTH,
                                          DATAS_PER_LINE, NB_WAYS,
                                          CACHE_SIZE_BYTES);
  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype way_selector_t is std_logic_vector(0 to cs.nb_ways - 1);
  subtype counter_t is natural range 0 to NB_WAYS - 1;
  subtype tag_slv_t is std_logic_vector(cs.tag_slv_width - 1 downto 0);
  type tag_slv_vector is array(0 to NB_WAYS - 1) of tag_slv_t;

  -- Eviction types
  --- eviction_entry = [ [alloc_counter] ]
  constant alloc_entry_len : natural := ((cs.ways_width * cs.nb_ways + 7) / 8) * 8;
  subtype alloc_entry_t is std_logic_vector(0 to alloc_entry_len - 1);

  -- Tag Data Memory interface
  subtype mem_addr_t is std_logic_vector(cs.index_width - 1 downto 0);
  signal mem_raddr                : mem_addr_t     := (others => '0');
  signal mem_read_data            : tag_slv_vector;
  signal mem_read_data_mux        : slv_2d(0 to cs.nb_ways - 1, cs.tag_slv_width - 1 downto 0);
  signal mem_read_data_validfound : std_ulogic_vector(0 to NB_WAYS - 1);
  signal mem_bypassed_rdata       : tag_slv_vector;
  signal mem_rren                 : way_selector_t := (others => '0');

  signal mem_waddr      : mem_addr_t;
  signal mem_wway       : natural range 0 to NB_WAYS - 1;
  signal mem_wren       : way_selector_t := (others => '0');
  signal mem_write_data : tag_slv_vector;

  -- Tag Allocate interface
  signal evict_rdata : alloc_entry_t := (others => '0');
  signal evict_wdata : alloc_entry_t := (others => '0');
  signal evict_wren  : std_logic     := '0';
  signal evict_way   : natural range 0 to NB_WAYS - 1;
  signal evict_wsel  : way_selector_t;

  signal mem_read_alloc_counter : alloc_entry_t;
  -- Debug signals

  -- Latched data for memory read + search
  signal mem_way_found     : natural range 0 to NB_WAYS - 1;
  signal search_wsel       : way_selector_t;

  ----------------------------------------------
  -- Functions for combinational memory logic --
  ----------------------------------------------
  function get_mem_addr(i_addr : addr_t) return mem_addr_t is
    variable index : natural range 0 to cs.nb_lines - 1;
  begin
    if slv_is_x(i_addr) then
      return (others => 'X');
    else
      index := get_address_index(i_addr, cs);
      return std_logic_vector(to_unsigned(index, cs.index_width));
    end if;
  end function get_mem_addr;

  function get_alloc_counter(ae : alloc_entry_t; step : natural)
    return natural is
    variable ac : natural;
  begin
    if slv_is_x(ae) then
      ac := 0;
    else
      ac := to_integer(unsigned(ae));
      ac := (ac + step) mod NB_WAYS;
    end if;
    return ac;
  end function get_alloc_counter;

begin  -- architecture str

  -- Memories instances
  tmem : for i in 0 to NB_WAYS - 1 generate
    tdata : sc_sram
      generic map (ADDR_WIDTH       => cs.index_width,
                   DATA_WIDTH       => cs.tag_slv_width,
                   READ_UNDER_WRITE => false,
                   LOOKAHEAD        => true,
                   DEBUG_NAME       => "tdata_w" & integer'image(i),
                   DEBUG            => DEBUG)
      port map (clk, mem_raddr, mem_waddr, mem_write_data(i),
                i_re, mem_wren(i), mem_read_data(i));
  end generate tmem;

  evictm : sc_sram
    generic map (ADDR_WIDTH       => cs.index_width,
                 DATA_WIDTH       => evict_wdata'length,
                 READ_UNDER_WRITE => false,
                 LOOKAHEAD        => false,
                 DEBUG_NAME       => "edata",
                 DEBUG            => DEBUG)
    port map (clk, mem_raddr, mem_waddr, evict_wdata,
              i_re, evict_wren, evict_rdata);


  reader : process(clk, i_raddr, mem_raddr, mem_read_data, i_re)
    variable te : tag_slv_t;
  begin
    mem_raddr <= get_mem_addr(i_raddr);

    for i in 0 to NB_WAYS - 1 loop
      te := mem_read_data(i);
      if rising_edge(clk) and i_re = '1' then
        if get_address_tag(i_raddr, cs) = get_tag(te, cs) and
          get_tag_valid(te, cs) = '1' then
          mem_read_data_validfound(i) <= '1';
        else
          mem_read_data_validfound(i) <= '0';
        end if;
      end if;
    end loop;

  end process reader;

  gsearch_wsel : for i in search_wsel'range generate
    search_wsel(i) <= mem_read_data_validfound(i);
  end generate;

  mem_way_found <= to_way(search_wsel);
  evict_way     <= get_alloc_counter(evict_rdata, 0);
  evict_wsel    <= to_way_selector(get_alloc_counter(evict_rdata, 0), cs);

  o_rway      <= search_wsel;
  o_way_found <= mem_way_found;
  o_way_evict <= evict_way;
  o_found     <= or_reduce(mem_read_data_validfound) = '1';

  mem_read_datas : process(mem_read_data)
  begin
    for i in mem_read_data'range loop
      assign_row(mem_read_data_mux, mem_read_data(i), i);
    end loop;
  end process mem_read_datas;

  o_tag_entry_mux : slv_2d_mux
    generic map (NB => NB_WAYS, WIDTH => o_tag_slv'length)
    port map (mem_read_data_mux, to_way(search_wsel), o_tag_slv);

  o_etag_entry_mux : slv_2d_mux
    generic map (NB => NB_WAYS, WIDTH => o_evict_tag_slv'length)
    port map (mem_read_data_mux, to_way(evict_wsel), o_evict_tag_slv);

  o_evict_dirty <= get_tag_dirty(mem_read_data(evict_way), cs) = '1';

  writer : process(i_wway, i_waddr, i_wtag_slv)
  begin
    mem_waddr <= get_mem_addr(i_waddr);
    mem_wren  <= i_wway;
    for way in mem_write_data'range loop
      mem_write_data(way) <= i_wtag_slv;
    end loop;
  end process writer;

  evicter : process(clk, i_evict_compute, evict_rdata)
  begin
    if rising_edge(clk) then
      evict_wren <= i_evict_compute;
      evict_wdata <=
        std_logic_vector(to_unsigned(get_alloc_counter(evict_rdata, 1), evict_rdata'length));
    end if;
  end process evicter;

end architecture str;
