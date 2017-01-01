-------------------------------------------------------------------------------
-- Title      : Cache definitions
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : cache_defs.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-15
-- Last update: 2017-01-01
-- Platform   : 
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-15  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

-------------------------------------------------------------------------------

package cache_defs is
  -- Constants for Singleport_Associative_Cache_tb
  constant ADDR_WIDTH           : natural := 32;
  constant DATA_WIDTH           : natural := 32;
  constant DATAS_PER_LINE_WIDTH : natural := 1;
  constant INDEX_WIDTH          : natural := 1;
  constant WAYS_WIDTH           : natural := 1;

  --constant ADDR_WIDTH           : natural := 32;
  --constant DATA_WIDTH           : natural := 32;
  --constant DATAS_PER_LINE_WIDTH : natural := 4;
  --constant INDEX_WIDTH          : natural := 7;
  --constant WAYS_WIDTH           : natural := 2;

  -- Infered constants
  constant NB_LINES             : natural := 2**INDEX_WIDTH;
  constant DATAS_PER_LINE       : natural := 2**DATAS_PER_LINE_WIDTH;
  constant NB_WAYS              : natural := 2**WAYS_WIDTH;

  constant ADDR_DATA_NBITS     : natural := integer(log2(real(DATA_WIDTH / 8)));
  constant ADDR_DATALINE_NBITS : natural :=
    ADDR_DATA_NBITS + DATAS_PER_LINE_WIDTH + INDEX_WIDTH;
  constant TAG_WIDTH : natural := ADDR_WIDTH - ADDR_DATALINE_NBITS;

  subtype addr_t is std_logic_vector(ADDR_WIDTH - 1 downto 0);
  subtype data_t is std_logic_vector(DATA_WIDTH - 1 downto 0);

  type data_vector is array(natural range <>) of data_t;

  subtype cache_line_selector_t is std_logic_vector(0 to DATAS_PER_LINE - 1);
  subtype way_selector_t is std_logic_vector(0 to NB_WAYS - 1);

  subtype cache_line_t is data_vector(0 to DATAS_PER_LINE - 1);
  subtype flat_cache_line_t is
    std_logic_vector(DATAS_PER_LINE * DATA_WIDTH - 1 downto 0);

  type cache_line_vector is array (natural range <>) of cache_line_t;
  type flat_cache_line_vector is array (natural range <>) of flat_cache_line_t;

  type cls_op is (cls_none, cls_refill, cls_flush);
  type cache_request_t is record
    req   : cls_op;
    addr  : addr_t;
    sel   : cache_line_selector_t;
    cline : cache_line_t;
  end record;

  type cache_response_t is record
    cline : cache_line_t;
    sel   : cache_line_selector_t;
    rdy   : std_logic;
    done  : std_logic;
  end record;

  -- Tag entry
  --- entry = [ [valid_bits_of_line] [dirty_bits_of_line] [tag_context] [tag]
  --- +-------------------------------------------------------+
  --- | Tag context | Dirty line bits | Valid line bits | Tag |
  --- +-------------------------------------------------------+
  subtype tag_t is std_logic_vector(TAG_WIDTH - 1 downto 0);
  subtype tag_context_t is std_logic_vector(1 downto 0);
  type tag_entry_t is record
    ctxt   : tag_context_t;
    valids : cache_line_selector_t;
    dirtys : cache_line_selector_t;
    tag    : tag_t;
  end record;
  type tag_entry_vector is array(natural range <>) of tag_entry_t;
  constant TAG_ENTRY_EMPTY : tag_entry_t := (
    (others => '0'), (others => '0'), (others => '0'), (others => '0'));

  -- Eviction types
  --- eviction_entry = [ [alloc_counter] ]
  constant alloc_entry_len : natural := ((WAYS_WIDTH * NB_WAYS + 7) / 8) * 8;
  subtype alloc_entry_t is std_logic_vector(0 to alloc_entry_len - 1);

  type mem_tag_status_t is record
    valid : std_logic;
    dirty : std_logic;
  end record;

  type cache_state is (s_idle, s_searching,
                       s_prepare_flushing, s_flush_outer, s_flushing,
                       s_refill_memory, s_refill_cache,
                       s_writethrough, s_write_allocate);

  -- Cache statistics
  type cache_stats_t is record
    read_hits      : natural;
    write_hits     : natural;
    read_misses    : natural;
    write_misses   : natural;
    write_throughs : natural;
    write_backs    : natural;
    flushes        : natural;
    refills        : natural;
  end record cache_stats_t;

  function get_address_index(i_address : std_logic_vector) return natural;

  function get_address_tag(i_address : std_logic_vector)
    return std_logic_vector;

  function get_data_set_index(i_address : std_logic_vector)
    return natural;

  function get_address(tag          : tag_t; index : natural range 0 to NB_LINES - 1;
                       data_in_line : natural range 0 to DATAS_PER_LINE - 1)
    return addr_t;

  function data_is_valid(addr : addr_t; te : tag_entry_t) return std_logic;

  function dataline_is_dirty(te : tag_entry_t) return boolean;

  function to_way_selector(way : natural range 0 to NB_WAYS - 1)
    return way_selector_t;

  function to_cacheline_selector(addr : addr_t) return cache_line_selector_t;

end package cache_defs;

package body cache_defs is

  function get_address_index(i_address : std_logic_vector) return natural is
    variable idx : natural;
  begin
    idx := to_integer(unsigned(i_address(
      ADDR_DATALINE_NBITS - 1 downto DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS)));
    return idx;
  end function get_address_index;

  function get_address_tag(i_address : std_logic_vector)
    return std_logic_vector is
    variable tag : tag_t;
  begin
    tag :=
      i_address(i_address'length - 1 downto i_address'length - TAG_WIDTH);
    return tag;
  end function get_address_tag;

  function get_data_set_index(i_address : std_logic_vector)
    return natural is
    variable set_index : natural range 0 to DATAS_PER_LINE - 1;
  begin
    if DATAS_PER_LINE = 1 then
      set_index := 0;
    else
      set_index := to_integer(unsigned(i_address(
        DATAS_PER_LINE_WIDTH + ADDR_DATA_NBITS - 1 downto ADDR_DATA_NBITS)));
    end if;
    return set_index;
  end function get_data_set_index;

  function get_address(tag          : tag_t; index : natural range 0 to NB_LINES - 1;
                       data_in_line : natural range 0 to DATAS_PER_LINE - 1)
    return addr_t is
  begin
    return tag & std_logic_vector(to_unsigned(index, INDEX_WIDTH) &
                                  to_unsigned(data_in_line, DATAS_PER_LINE_WIDTH) &
                                  to_unsigned(0, ADDR_DATA_NBITS));
  end function get_address;

  function data_is_valid(addr : addr_t; te : tag_entry_t) return std_logic is
    variable data_in_line : natural range 0 to DATAS_PER_LINE - 1;
  begin
    data_in_line := get_data_set_index(addr);
    return te.valids(data_in_line);
  end function data_is_valid;

  function dataline_is_dirty(te : tag_entry_t) return boolean is
    constant z : cache_line_selector_t := (others => '0');
    variable o : boolean;
  begin
    o := te.dirtys /= z;
    return o;
  end function dataline_is_dirty;

  function to_way_selector(way : natural range 0 to NB_WAYS - 1)
    return way_selector_t is
    variable ws : way_selector_t := (others => '0');
  begin
    for i in ws'range loop
      if i = way then
        ws(i) := '1';
      end if;
    end loop;
    return ws;
  end function to_way_selector;

  function to_cacheline_selector(addr : addr_t) return cache_line_selector_t is
    variable clsel        : cache_line_selector_t := (others => '0');
    variable data_in_line : natural range 0 to DATAS_PER_LINE - 1;
  begin
    data_in_line        := get_data_set_index(addr);
    clsel(data_in_line) := '1';
    return clsel;
  end function to_cacheline_selector;

end package body cache_defs;

