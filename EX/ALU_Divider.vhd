-------------------------------------------------------------------------------
-- Title      : ALU divider
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : ALU_Divider.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-06
-- Last update: 2017-01-09
-- Platform   : 
-- Standard   : VHDL'93/02
-------------------------------------------------------------------------------
-- Description: 
-------------------------------------------------------------------------------
-- Copyright (c) 2016 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2016-12-06  1.0      rj      Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-------------------------------------------------------------------------------

entity ALU_Divider is
  generic (
    DATA_WIDTH : integer
    );

  port (
    clk        : in  std_logic;
    clkena     : in  std_logic;
    i_ra       : in  unsigned(DATA_WIDTH - 1 downto 0);
    i_rb       : in  unsigned(DATA_WIDTH - 1 downto 0);
    i_div_by_0 : in  std_logic;
    o_rdy      : out std_logic;
    o_q        : out unsigned(DATA_WIDTH * 2 - 1 downto 0)
    );
end entity ALU_Divider;

architecture str of ALU_Divider is
  constant r_unknown : unsigned(DATA_WIDTH - 1 downto 0) := (others => 'X');
  signal quotient    : unsigned(DATA_WIDTH - 1 downto 0);
  signal remain      : unsigned(DATA_WIDTH - 1 downto 0);
  signal div_by_0    : boolean                           := true;

  function get_quotient(signal a        : in unsigned(DATA_WIDTH - 1 downto 0);
                        signal b        : in unsigned(DATA_WIDTH - 1 downto 0);
                        signal div_by_0 : in boolean)
    return unsigned is
  begin
    if div_by_0 then
      return to_unsigned(0, DATA_WIDTH);
    else
      return a / b;
    end if;
  end function get_quotient;

  function get_remain(signal a        : in unsigned(DATA_WIDTH - 1 downto 0);
                      signal b        : in unsigned(DATA_WIDTH - 1 downto 0);
                      signal div_by_0 : in boolean)
    return unsigned is
  begin
    if div_by_0 then
      return to_unsigned(0, DATA_WIDTH);
    else
      return a rem b;
    end if;
  end function get_remain;

begin  -- architecture str

  div_by_0 <= false when i_div_by_0 = '0' else true;
  remain   <= get_remain(i_ra, i_rb, div_by_0);
  quotient <= get_quotient(i_ra, i_rb, div_by_0);

  div : process(clk, clkena)
  begin
    if clkena = '1' and rising_edge(clk) then
      o_q <= remain & quotient;
    end if;
  end process div;

end architecture str;
