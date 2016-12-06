-------------------------------------------------------------------------------
-- Title      : ALU divider
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : ALU_Divider.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-06
-- Last update: 2016-12-06
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
    i_ra       : in  unsigned(DATA_WIDTH - 1 downto 0);
    i_rb       : in  unsigned(DATA_WIDTH - 1 downto 0);
    o_q        : out unsigned(DATA_WIDTH * 2 - 1 downto 0);
    o_div_by_0 : out std_logic
    );
end entity ALU_Divider;

-------------------------------------------------------------------------------

architecture rtl of ALU_Divider is
  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  constant r_unknown : unsigned(DATA_WIDTH - 1 downto 0) := (others => 'X');
  signal quotient    : unsigned(DATA_WIDTH - 1 downto 0);
  signal remain      : unsigned(DATA_WIDTH - 1 downto 0);
  signal div_by_0    : boolean                           := true;

  function get_quotient(signal a : in unsigned(DATA_WIDTH - 1 downto 0);
                        signal b : in unsigned(DATA_WIDTH - 1 downto 0))
    return unsigned is
  begin
    if b = to_unsigned(0, DATA_WIDTH) then
      return to_unsigned(0, DATA_WIDTH);
    else
      return a / b;
    end if;
  end function get_quotient;

  function get_remain(signal a : in unsigned(DATA_WIDTH - 1 downto 0);
                      signal b : in unsigned(DATA_WIDTH - 1 downto 0))
    return unsigned is
  begin
    if b = to_unsigned(0, DATA_WIDTH) then
      return to_unsigned(0, DATA_WIDTH);
    else
      return a rem b;
    end if;
  end function get_remain;

begin  -- architecture rtl

  div_by_0 <= true when i_rb = to_unsigned(0, DATA_WIDTH) else false;
  remain   <= get_remain(i_ra, i_rb);
  quotient <= get_quotient(i_ra, i_rb);

  o_q        <= remain & quotient when not div_by_0;
  o_div_by_0 <= '1'               when div_by_0 else '0';

end architecture rtl;

-------------------------------------------------------------------------------
