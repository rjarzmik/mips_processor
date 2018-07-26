-------------------------------------------------------------------------------
-- Title      : Cache sizings
-- Project    : Cache
-------------------------------------------------------------------------------
-- File       : cache_sizing.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2016-02-13
-- Last update: 2017-02-13
-- Platform   :
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Common structure to expose all sizes for a cache
--
-- Address anatomy :
-- +---------------+-------+------------+---------+
-- |      Tag      | Index | DataInLine | OneData |
-- +---------------+-------+------------+---------+
--
-- Example of address with 128 indexes, 64 bytes per line, data = 32 bits,
--                    2 ways, ie. 16kBytes cache
--  31           13 12    6 5          2 1       0
-- +---------------+-------+------------+---------+
-- |      Tag      | Index | DataInLine | OneData |
-- +---------------+-------+------------+---------+
--
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2016-02-13  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package cache_sizing is
  type csizes is record
    addr_width           : natural;
    data_width           : natural;
    datas_per_line       : natural;
    nb_ways              : natural;
    cache_size_bytes     : natural;
    -- Inferred datas
    datas_per_line_width : natural;
    index_width          : natural;
    ways_width           : natural;
    nb_lines             : natural;
    addr_data_nbits      : natural;
    addr_dataline_nbits  : natural;
    tag_width            : natural;
    tag_slv_width        : natural;
  end record csizes;

  function to_cache_sizing(addr_width, data_width : natural;
                           datas_per_line, nb_ways, cache_size_bytes : natural)
    return csizes;

  function get_tag_slv_width(addr_width, data_width, datas_per_line, nb_ways,
                             cache_size_bytes : positive)
    return positive;

end package cache_sizing;

--------------------------------------------------------------------------------

package body cache_sizing is

  function to_cache_sizing(addr_width, data_width : natural;
                           datas_per_line, nb_ways, cache_size_bytes : natural)
    return csizes is
    variable cs : csizes;
  begin
    cs.addr_width := addr_width;
    cs.data_width := data_width;
    cs.datas_per_line := datas_per_line;
    cs.nb_ways := nb_ways;
    cs.cache_size_bytes := cache_size_bytes;

    cs.datas_per_line_width := integer(log2(real(datas_per_line)));
    cs.ways_width := integer(log2(real(nb_ways)));
    cs.addr_data_nbits := integer(log2(real(data_width / 8)));
    cs.addr_dataline_nbits := cs.addr_data_nbits + cs.datas_per_line_width;
                              
    cs.index_width := integer(log2(real(cache_size_bytes / nb_ways))) -
                              cs.addr_dataline_nbits;
    cs.tag_width := addr_width - cs.index_width - cs.addr_dataline_nbits;
    cs.nb_lines := 2**cs.index_width;
    cs.tag_slv_width := cs.tag_width + 1 + 1 + 1; -- valid, dirty and context bits

    return cs;
  end function to_cache_sizing;

  function get_tag_slv_width(addr_width, data_width, datas_per_line, nb_ways,
                             cache_size_bytes : positive)
    return positive is
    variable cs : csizes;
  begin
    cs := to_cache_sizing(addr_width, data_width, datas_per_line, nb_ways, cache_size_bytes);
    return cs.tag_slv_width;
  end function get_tag_slv_width;

end package body cache_sizing;
