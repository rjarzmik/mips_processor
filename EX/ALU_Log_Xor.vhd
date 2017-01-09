-------------------------------------------------------------------------------
-- Title      : ALU log_xor
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : ALU_Log_Xor.vhd
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

entity ALU_Log_Xor is
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
end entity ALU_Log_Xor;

architecture str of ALU_Log_Xor is
  constant upper : unsigned(DATA_WIDTH - 1 downto 0) := (others => '0');
begin  -- architecture str

  logxor : process(clk, clkena)
  begin
    if clkena = '1' and rising_edge(clk) then
      o_q <= upper & (i_ra xor i_rb);
    end if;
  end process logxor;

end architecture str;
