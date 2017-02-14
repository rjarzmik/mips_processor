-------------------------------------------------------------------------------
-- Title      : std_logic_vector utilities
-- Project    : 
-------------------------------------------------------------------------------
-- File       : slv_utils.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    :
-- Created    : 2017-01-30
-- Last update: 2017-02-18
-- Platform   :
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: Various utilities
-------------------------------------------------------------------------------
-- Copyright (c) 2017 Robert Jarzmik <robert.jarzmik@free.fr>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2017-01-30  1.0      robert.jarzmik@free.fr  Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package slv_utils is

  function slv_is_x(slv : std_logic_vector) return boolean;

  function slv_is_x(slv : unsigned) return boolean;

  type slv_2d is array(natural range <>, natural range <>) of std_logic;

  function and_reduce(a : std_ulogic_vector) return std_ulogic;

  function or_reduce(a : std_ulogic_vector) return std_ulogic;

  function get_row(slm : slv_2d; row : natural) return std_logic_vector;
  procedure assign_row(signal slm : out slv_2d; slv : in std_logic_vector;
                       constant row : natural);

  component slv_2d_mux is
    generic (
      WIDTH : positive;
      NB    : positive);
    port (
      datas : in  slv_2d(0 to NB - 1, WIDTH - 1 downto 0);
      sel   : in  natural range 0 to NB - 1;
      q     : out std_logic_vector(WIDTH - 1 downto 0));
  end component slv_2d_mux;

end package slv_utils;
