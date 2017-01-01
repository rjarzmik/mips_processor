-------------------------------------------------------------------------------
-- Title      : Tag entry mux
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : generic_mux.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-25
-- Last update: 2016-12-25
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-25  1.0      rj	Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.cache_defs.tag_entry_t;
use work.cache_defs.tag_entry_vector;

entity tag_entry_mux is

  generic (
    NB : natural
    );

  port (
    index   : in  natural range 0 to NB - 1;
    a       : in  tag_entry_vector(0 to NB - 1);
    q       : out tag_entry_t
    );

end entity tag_entry_mux;

architecture str of tag_entry_mux is
begin

  q <= a(index);

end architecture str;
