-------------------------------------------------------------------------------
-- Title      : Cache tags data memory
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : tags_data_mem.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-15
-- Last update: 2017-01-01
-- Platform   : 
-- Standard   : VHDL'93/02
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

use work.cache_defs.all;

-------------------------------------------------------------------------------

entity tags_data_mem is
  generic
    (
      DEBUG : boolean := false
      );

  port (
    clk               : in  std_logic;
    -- Reader
    i_re              : in  std_logic;
    i_raddr           : in  addr_t;
    o_tag_found       : out boolean;
    o_way_found       : out natural range 0 to NB_WAYS - 1;
    --- Combinational : read_tag_entries(i_raddr)(way)
    o_tag_entry       : out tag_entry_t;
    o_data_valid      : out std_logic;
    o_way_evict       : out natural range 0 to NB_WAYS - 1;
    o_evict_dirty     : out boolean;
    o_evict_tag_entry : out tag_entry_t;

    -- Writer
    --- i_update_addr will be the one latched when i_prepare_we = 1
    --- base data for update will be the one latched from mem_read_data the
    --- cycle _after_ i_prepare_we = 1
    i_waddr         : in addr_t;
    i_we            : in std_logic;
    i_update_way    : in natural range 0 to NB_WAYS - 1;
    i_wtag_entry    : in tag_entry_t;
    --- i_way_alloc triggers a new election of the next to evict line
    i_evict_compute : in std_logic
    );

end entity tags_data_mem;

architecture str of tags_data_mem is

  -- Entry anatomy, ie. entry for a given address index
  subtype counter_t is natural range 0 to NB_WAYS - 1;

  -- Tag Data Memory interface
  subtype mem_addr_t is std_logic_vector(INDEX_WIDTH - 1 downto 0);
  signal mem_raddr          : mem_addr_t                         := (others => '0');
  signal mem_read_data      : tag_entry_vector(0 to NB_WAYS - 1) := (others => TAG_ENTRY_EMPTY);
  signal mem_bypassed_rdata : tag_entry_vector(0 to NB_WAYS - 1) := (others => TAG_ENTRY_EMPTY);
  signal mem_rren           : way_selector_t                     := (others => '0');

  signal mem_waddr      : mem_addr_t;
  signal mem_we         : std_logic;
  signal mem_wway       : natural range 0 to NB_WAYS - 1;
  signal mem_wren       : way_selector_t                     := (others => '0');
  signal mem_write_data : tag_entry_vector(0 to NB_WAYS - 1) := (others => TAG_ENTRY_EMPTY);

  -- Tag Allocate interface
  signal evict_rdata          : alloc_entry_t := (others => '0');
  signal evict_wdata          : alloc_entry_t := (others => '0');
  signal evict_bypassed_rdata : alloc_entry_t := (others => '0');
  signal evict_wren           : std_logic     := '0';
  signal evict_way            : natural range 0 to NB_WAYS - 1;

  signal mem_read_alloc_counter : alloc_entry_t;
  -- Debug signals

  -- Latched data for memory read + search
  signal searched_addr     : addr_t      := (others => '0');
  signal mem_w_bypass      : boolean     := false;
  signal mem_te            : tag_entry_t := TAG_ENTRY_EMPTY;
  signal mem_searched_addr : mem_addr_t  := (others => '0');
  signal mem_way_found     : natural range 0 to NB_WAYS - 1;

  ----------------------------------------------
  -- Functions for combinational memory logic --
  ----------------------------------------------
  function get_is_way_found(addr : addr_t;
                            tev  : tag_entry_vector(0 to NB_WAYS - 1))
    return boolean is
    variable needle    : tag_t;
    variable way_found : boolean := false;
  begin
    needle := get_address_tag(addr);
    for way in tev'range loop
      if tev(way).tag = needle and tev(way).valids /= TAG_ENTRY_EMPTY.valids then
        way_found := true;
      end if;
    end loop;
    return way_found;
  end function get_is_way_found;

  function get_way_found(addr : addr_t;
                         tev  : tag_entry_vector(0 to NB_WAYS - 1))
    return natural is
    variable needle : tag_t;
    variable oway   : natural range 0 to NB_WAYS - 1 := 0;
  begin
    needle := get_address_tag(addr);
    for way in tev'range loop
      if tev(way).tag = needle and tev(way).valids /= TAG_ENTRY_EMPTY.valids then
        oway := way;
      end if;
    end loop;
    return oway;
  end function get_way_found;

  function get_alloc_counter(ae : alloc_entry_t; step : natural)
    return natural is
    variable ac : natural;
  begin
    ac := to_integer(unsigned(ae));
    ac := (ac + step) mod NB_WAYS;
    return ac;
  end function get_alloc_counter;

  function get_updated_tag_entry(addr         : addr_t;
                                 a            : tag_entry_t;
                                 data_in_line : natural range 0 to DATAS_PER_LINE;
                                 valid        : std_ulogic;
                                 dirty        : std_ulogic;
                                 ctxt         : tag_context_t)
    return tag_entry_t is
    variable te : tag_entry_t;
  begin
    te.ctxt                 := ctxt;
    te.valids(data_in_line) := valid;
    te.dirtys(data_in_line) := dirty;
    return te;
  end function get_updated_tag_entry;

begin  -- architecture str

  -- Memories instances
  tmem : for i in 0 to NB_WAYS - 1 generate
    tdata : entity work.memory_tagmem_internal
      generic map (ADDR_WIDTH => INDEX_WIDTH, DEBUG_IDX => i, DEBUG => DEBUG)
      port map (clk, mem_raddr, mem_waddr, mem_write_data(i), i_re,
                mem_wren(i), mem_read_data(i));
  end generate tmem;

  evictm : entity work.memory_eviction_internal
    generic map (ADDR_WIDTH => INDEX_WIDTH, DEBUG => DEBUG)
    port map (clk, mem_raddr, mem_waddr, evict_wdata, i_re,
              evict_wren, evict_rdata);

  mem_raddr <= std_logic_vector(to_unsigned(get_address_index(i_raddr), mem_raddr'length));

  reader : process(clk, i_raddr)
  begin
    if rising_edge(clk) then
      searched_addr     <= i_raddr;
      mem_searched_addr <= mem_raddr;
    end if;
  end process reader;

  mem_w_bypass <= mem_searched_addr = mem_waddr and mem_we = '1';
  gmbypassrdata : for way in mem_bypassed_rdata'range generate
    mem_bypassed_rdata(way) <= mem_write_data(way) when mem_w_bypass and way = mem_wway
                               else mem_read_data(way);
  end generate;

  evict_bypassed_rdata <= evict_wdata when evict_wren = '1' and mem_waddr = mem_searched_addr else evict_rdata;

  -- mem_way_found <= get_way_found(searched_addr, mem_read_data);
  mem_way_found <= get_way_found(searched_addr, mem_bypassed_rdata);
  mem_te        <= mem_bypassed_rdata(mem_way_found);
  evict_way     <= get_alloc_counter(evict_bypassed_rdata, 0);

  o_tag_found  <= get_is_way_found(searched_addr, mem_bypassed_rdata);
  o_way_found  <= mem_way_found;
  o_way_evict  <= evict_way;
  o_data_valid <= data_is_valid(searched_addr, mem_te);

  o_tag_entry_mux : entity work.tag_entry_mux
    generic map (NB => NB_WAYS)
    port map (mem_way_found, mem_bypassed_rdata, o_tag_entry);

  o_evict_dirty <= dataline_is_dirty(mem_bypassed_rdata(evict_way));

  o_evict_tag_entry_mux : entity work.tag_entry_mux
    generic map (NB => NB_WAYS)
    port map (evict_way, mem_bypassed_rdata, o_evict_tag_entry);

  writer : process(clk, mem_searched_addr, i_we, i_update_way, i_waddr)
  begin
    if rising_edge(clk) then
      mem_waddr <= std_logic_vector(to_unsigned(get_address_index(i_waddr), mem_waddr'length));
      mem_we    <= i_we;
      mem_wway  <= i_update_way;
      for way in mem_wren'range loop
        if i_we = '1' and i_update_way = way then
          mem_wren(way) <= '1';
        else
          mem_wren(way) <= '0';
        end if;
      end loop;
      for way in mem_write_data'range loop
        mem_write_data(way) <= i_wtag_entry;
      end loop;
    end if;
  end process writer;

  evicter : process(clk, mem_searched_addr, i_evict_compute)
  begin
    if rising_edge(clk) then
      evict_wren <= i_evict_compute;
      evict_wdata <=
        std_logic_vector(to_unsigned(get_alloc_counter(evict_rdata, 1), evict_wdata'length));
    end if;
  end process evicter;

end architecture str;
