-------------------------------------------------------------------------------
-- Title      : ALU adder
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : ALU_Adder.vhd
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

entity ALU_Adder is
  generic (
    DATA_WIDTH : integer
    );

  port (
    clk    : in  std_logic;
    clkena : in  std_logic;
    i_ra   : in  unsigned(DATA_WIDTH - 1 downto 0);
    i_rb   : in  unsigned(DATA_WIDTH - 1 downto 0);
    o_q    : out unsigned(DATA_WIDTH * 2 - 1 downto 0)
    );
end entity ALU_Adder;

architecture str of ALU_Adder is
  signal result      : unsigned(DATA_WIDTH downto 0);
  signal sign_extend : unsigned(DATA_WIDTH - 2 downto 0);

begin  -- architecture rtl

  add : process(clk, clkena)
  begin
    if clkena = '1' and rising_edge(clk) then
      o_q <= sign_extend & result;
    end if;
  end process add;

  result      <= resize(i_ra, DATA_WIDTH + 1) + resize(i_rb, DATA_WIDTH + 1);
  sign_extend <= (others => result(DATA_WIDTH));

end architecture str;
