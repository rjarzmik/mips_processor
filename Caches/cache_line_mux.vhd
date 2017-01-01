-------------------------------------------------------------------------------
-- Title      : Cache line muxer
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : cache_line_mux.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-23
-- Last update: 2016-12-23
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-23  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.cache_line_t;
use work.cache_defs.cache_line_vector;
use work.cache_defs.flat_cache_line_vector;
use work.cache_defs.flat_cache_line_t;

-------------------------------------------------------------------------------

entity cache_line_mux is

  generic (
    NB : natural
    );

  port (
    index   : in  natural range 0 to NB - 1;
    fclines : in  flat_cache_line_vector(0 to NB - 1);
    q       : out flat_cache_line_t
    );

end entity cache_line_mux;

-------------------------------------------------------------------------------

architecture str of cache_line_mux is

begin  -- architecture str

  q <= fclines(index);

end architecture str;

-------------------------------------------------------------------------------
