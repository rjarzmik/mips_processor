-------------------------------------------------------------------------------
-- Title      : ALU multiplier
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : ALU_Multiplier.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-06
-- Last update: 2017-01-15
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

package mul_utils is
  function get_low(a                                         : unsigned) return unsigned;
  function get_high(a                                        : unsigned) return unsigned;
  function get_result4(axb_high, axb_low, axb_med1, axb_med2 : unsigned)
    return unsigned;
end package mul_utils;

package body mul_utils is
  function get_low(a : unsigned) return unsigned is
  begin
    return a(a'length / 2 - 1 downto 0);
  end function get_low;

  function get_high(a : unsigned) return unsigned is
  begin
    return a(a'length - 1 downto a'length / 2);
  end function get_high;

  function get_result4(axb_high, axb_low, axb_med1, axb_med2 : unsigned)
    return unsigned is
    constant half0 : unsigned(axb_low'length / 2 - 1 downto 0) := (others => '0');
    variable q     : unsigned(axb_low'length * 2 - 1 downto 0);
  begin
    q := axb_high & axb_low + (axb_med1 & half0) + (axb_med2 & half0);
    return q;
  end function get_result4;

end package body mul_utils;

--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mul_utils.all;

entity mul_NxN is
  generic (DATA_WIDTH : natural);
  port (clk    : in  std_logic;
        clkena : in  std_logic;
        a, b   : in  unsigned(DATA_WIDTH -1 downto 0);
        q      : out unsigned(DATA_WIDTH * 2 - 1 downto 0));
end mul_NxN;

architecture combi of mul_NxN is
  constant half0 : unsigned(a'length / 2 - 1 downto 0) := (others => '0');

  signal axb_low  : unsigned(a'length - 1 downto 0);
  signal axb_med1 : unsigned(a'length - 1 downto 0);
  signal axb_med2 : unsigned(a'length - 1 downto 0);
  signal axb_high : unsigned(a'length - 1 downto 0);
begin
  axb_high <= get_high(a) * get_high(b);
  axb_med1 <= get_high(a) * get_low(b);
  axb_med2 <= get_low(a) * get_high(b);
  axb_low  <= get_low(a) * get_low(b);
  q        <= axb_high & axb_low + (axb_med1 & half0) + (axb_med2 & half0);
end architecture combi;

architecture pipeline of mul_NxN is
  constant half0 : unsigned(a'length / 2 - 1 downto 0) := (others => '0');

  signal axb_low  : unsigned(a'length - 1 downto 0);
  signal axb_med1 : unsigned(a'length - 1 downto 0);
  signal axb_med2 : unsigned(a'length - 1 downto 0);
  signal axb_high : unsigned(a'length - 1 downto 0);
begin
  process(clk)
  begin
    if clkena = '1' and rising_edge(clk) then
      axb_high <= get_high(a) * get_high(b);
      axb_med1 <= get_high(a) * get_low(b);
      axb_med2 <= get_low(a) * get_high(b);
      axb_low  <= get_low(a) * get_low(b);

      q <= get_result4(axb_high, axb_low, axb_med1, axb_med2);
    end if;
  end process;
end architecture pipeline;

--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mul_utils.all;

entity mul_16x16 is
  port (clk    : in  std_logic;
        clkena : in  std_logic;
        a, b   : in  unsigned(16 - 1 downto 0);
        q      : out unsigned(16 * 2 - 1 downto 0));
end mul_16x16;

architecture combi of mul_16x16 is
begin
    q <= a * b;
end architecture combi;

architecture pipeline of mul_16x16 is
begin
  process(clk, clkena)
  begin
    if rising_edge(clk) and clkena = '1' then
      q <= a * b;
    end if;
  end process;
end architecture pipeline;

--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mul_utils.all;

entity ALU_Multiplier is
  generic (
    DATA_WIDTH : integer := 32
    );

  port (
    clk    : in  std_logic;
    clkena : in  std_logic;
    i_ra   : in  unsigned(DATA_WIDTH - 1 downto 0);
    i_rb   : in  unsigned(DATA_WIDTH - 1 downto 0);
    o_q    : out unsigned(DATA_WIDTH * 2 - 1 downto 0)
    );
end entity ALU_Multiplier;

architecture str of ALU_Multiplier is
  signal axb_low, axb_med1, axb_med2, axb_high : unsigned(DATA_WIDTH - 1 downto 0);
  signal a, b : unsigned(DATA_WIDTH - 1 downto 0);
begin  -- architecture str
  mul_16x16_1: entity work.mul_16x16(pipeline)
    port map (
      clk    => clk,
      clkena => clkena,
      a      => get_low(a),
      b      => get_low(b),
      q      => axb_low);
  mul_16x16_2: entity work.mul_16x16(pipeline)
    port map (
      clk    => clk,
      clkena => clkena,
      a      => get_high(a),
      b      => get_low(b),
      q      => axb_med1);
  mul_16x16_3: entity work.mul_16x16(pipeline)
    port map (
      clk    => clk,
      clkena => clkena,
      a      => get_low(a),
      b      => get_high(b),
      q      => axb_med2);
  mul_16x16_4: entity work.mul_16x16(pipeline)
    port map (
      clk    => clk,
      clkena => clkena,
      a      => get_high(a),
      b      => get_high(b),
      q      => axb_high);

  process(clk)
  begin
    if clkena = '1' and rising_edge(clk) then
      a <= i_ra;
      b <= i_rb;
    end if;
  end process;

  process(clk)
  begin
    if clkena = '1' and rising_edge(clk) then
      o_q <= get_result4(axb_high, axb_low, axb_med1, axb_med2);
    end if;
  end process;

end architecture str;
