-------------------------------------------------------------------------------
-- Title      : Cache definitions
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : cache_defs.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-15
-- Last update: 2017-02-13
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

library rjarzmik;
use rjarzmik.slv_utils.slv_is_x;
use work.cache_sizing.csizes;

package cache_defs is
  -- Tag entry
  --- +-----------------------------------------------------+
  --- | Tag context | Dirty line bit | Valid line bit | Tag |
  --- +-----------------------------------------------------+
  function get_tag(tag_slv : std_logic_vector; constant cs : csizes)
    return std_logic_vector;

  function get_tag_valid(tag_slv : std_logic_vector; constant cs : csizes)
    return std_logic;

  function get_tag_dirty(tag_slv : std_logic_vector; constant cs : csizes)
    return std_logic;

  function to_tag_slv(tag   : std_logic_vector; valid : std_logic;
                      dirty : std_logic; ctxt : std_logic_vector)
    return std_logic_vector;

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

  function get_address_index(i_address : std_logic_vector; cs : csizes) return natural;

  function get_address_tag(i_address : std_logic_vector; cs : csizes)
    return std_logic_vector;

  function get_data_set_index(i_address : std_logic_vector; cs : csizes)
    return natural;

  function get_address(tag          : std_logic_vector;
                       index        : natural;
                       data_in_line : natural;
                       cs           : csizes)
    return std_logic_vector;

  function to_way_selector(way : natural; cs : csizes)
    return std_logic_vector;

  function to_way(wsel : std_logic_vector) return natural;

end package cache_defs;

package body cache_defs is

  function get_address_index(i_address : std_logic_vector; cs : csizes) return natural is
    variable idx : natural;
  begin
    if not slv_is_x(i_address) then
      idx := to_integer(unsigned(i_address(
        cs.addr_dataline_nbits + cs.index_width - 1 downto cs.addr_dataline_nbits)));
    else
      idx := 0;
    end if;
    return idx;
  end function get_address_index;

  function get_address_tag(i_address : std_logic_vector; cs : csizes)
    return std_logic_vector is
    variable tag : std_logic_vector(cs.tag_width - 1 downto 0);
  begin
    tag :=
      i_address(i_address'length - 1 downto i_address'length - cs.tag_width);
    return tag;
  end function get_address_tag;

  function get_data_set_index(i_address : std_logic_vector; cs : csizes)
    return natural is
    variable set_index : natural range 0 to cs.datas_per_line - 1;
  begin
    if cs.datas_per_line = 1 then
      set_index := 0;
    else
      set_index := to_integer(unsigned(i_address(
        cs.datas_per_line_width + cs.addr_data_nbits - 1 downto
        cs.addr_data_nbits)));
    end if;
    return set_index;
  end function get_data_set_index;

  function get_address(tag          : std_logic_vector;
                       index        : natural;
                       data_in_line : natural;
                       cs           : csizes)
    return std_logic_vector is
  begin
    return tag & std_logic_vector(to_unsigned(index, cs.index_width) &
                                  to_unsigned(data_in_line, cs.datas_per_line_width) &
                                  to_unsigned(0, cs.addr_data_nbits));
  end function get_address;

  function to_way_selector(way : natural; cs : csizes)
    return std_logic_vector is
    variable ws : std_logic_vector(0 to cs.nb_ways - 1) := (others => '0');
  begin
    for i in ws'range loop
      if i = way then
        ws(i) := '1';
      end if;
    end loop;
    return ws;
  end function to_way_selector;

  function compute_way_bit(wsel : std_logic_vector; idx : natural)
    return std_ulogic is
    variable i, j : natural;
    variable abit : std_ulogic := '0';
  begin
    i := 2**idx;
    while i < wsel'length loop
      for j in i to i + 2**idx - 1 loop
        abit := abit or wsel(j);
      end loop;
      i := i + 2**(2**idx);
    end loop;
    return abit;
  end function compute_way_bit;

  function compute_way(wsel : std_logic_vector) return std_logic_vector is
    constant olen : natural := integer(log2(real(wsel'length)));
    variable way  : std_logic_vector(olen - 1 downto 0);
  begin
    for i in way'range loop
      way(i) := compute_way_bit(wsel, i);
    end loop;
    return way;
  end function compute_way;

  function do_to_way(wsel : std_logic_vector)
    return natural is
    variable way   : natural;
    variable hlen  : natural;
    variable hzero : std_logic_vector(0 to wsel'length / 2 - 1);
    variable vlow  : std_logic_vector(0 to wsel'length / 2 - 1);
    variable vhigh : std_logic_vector(0 to wsel'length / 2 - 1);
  begin
    way := 0;
    if wsel'length = 2 then
      if wsel(0) = '1' then
        return 0;
      else
        return 1;
      end if;
    else
      hzero := (others => '0');
      hlen  := wsel'length / 2;
      vlow  := wsel(0 to hlen - 1);
      vhigh := wsel(hlen to wsel'length - 1);
      if vhigh = hzero then
        return do_to_way(vlow);
      else
        return hlen + do_to_way(vhigh);
      end if;
    end if;
  end function do_to_way;

  --function to_way(wsel : way_selector_t) return natural is
  --begin
  --  return do_to_way(wsel);
  --end function to_way;
  -- Trick: the ways should be traversed in inverse range.
  --   The reason is that upon initialization, the address 0 is matching all
  --   N ways. The first way to be written will be way 0, hence if all all
  --   ways are matching, way 0 should be returned.
  function to_way(wsel : std_logic_vector) return natural is
  begin
    if (wsel(0) and wsel(wsel'length - 1)) = '1' or slv_is_x(wsel) then
      return 0;
    else
      return to_integer(unsigned(compute_way(wsel)));
    end if;
  end function to_way;

  -- Tags
  function get_tag(tag_slv : std_logic_vector; constant cs : csizes)
    return std_logic_vector is
    variable o : std_logic_vector(cs.tag_width - 1 downto 0);
  begin
    o := tag_slv(o'length - 1 downto 0);
    return o;
  end function get_tag;

  function get_tag_valid(tag_slv : std_logic_vector; constant cs : csizes)
    return std_logic is
    variable o : std_logic;
  begin
    o := tag_slv(cs.tag_width);
    return o;
  end function get_tag_valid;

  function get_tag_dirty(tag_slv : std_logic_vector; constant cs : csizes)
    return std_logic is
    variable o : std_logic;
  begin
    o := tag_slv(cs.tag_width + 1);
    return o;
  end function get_tag_dirty;

  function to_tag_slv(tag   : std_logic_vector; valid : std_logic;
                      dirty : std_logic; ctxt : std_logic_vector)
    return std_logic_vector is
    variable slv_valid : std_logic_vector(0 downto 0);
    variable slv_dirty : std_logic_vector(0 downto 0);
  begin
    slv_valid := (others => valid);
    slv_dirty := (others => dirty);
    return ctxt & slv_dirty & slv_valid & tag;
  end function to_tag_slv;

end package body cache_defs;
