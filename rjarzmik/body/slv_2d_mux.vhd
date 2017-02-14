-------------------------------------------------------------------------------
-- Title      : Mux of standard logic vectors
-- Project    : Cache implementations
-------------------------------------------------------------------------------
-- File       : slv_2d_mux.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-02-12
-- Last update: 2017-02-13
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Muxer of std_logic_vectors
--   As VHDL seems utterly broken for unconstrained arrays of std_logic_vector,
--   even in VHDL'08, use a 2 dimension array as a passthrough structure to
--   pass arrays of std_logic_vector across entities ports.
--   The inpout slv_2d should be created with a assign_row() series of calls.
--
--   The muxer outputs the Nth std_logic_vector.
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-02-12  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library rjarzmik;
use rjarzmik.slv_utils.slv_2d;
use rjarzmik.slv_utils.get_row;

entity slv_2d_mux is
  generic(WIDTH : positive; NB : positive);
  port (
    datas : in  slv_2d(0 to NB - 1, WIDTH - 1 downto 0);
    sel   : in  natural range 0 to NB - 1;
    q     : out std_logic_vector(WIDTH - 1 downto 0)
    );
end entity slv_2d_mux;

architecture str of slv_2d_mux is

begin  -- architecture str
  q <= get_row(datas, sel);
end architecture str;
