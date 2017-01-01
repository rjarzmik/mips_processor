-------------------------------------------------------------------------------
-- Title      : Data bus muxer
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : data_t_mux.vhd
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

use work.cache_defs.data_t;
use work.cache_defs.data_vector;

entity data_t_mux is

  generic (
    NB : natural
    );

  port (
    index   : in  natural range 0 to NB - 1;
    a       : in  data_vector(0 to NB - 1);
    q       : out data_t
    );

end entity data_t_mux;

architecture str of data_t_mux is
begin

  q <= a(index);

end architecture str;

-------------------------------------------------------------------------------
