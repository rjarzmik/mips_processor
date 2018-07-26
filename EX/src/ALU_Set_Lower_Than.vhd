-------------------------------------------------------------------------------
-- Title      : ALU set lower than
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : ALU_Set_Lower_Than.vhd
-- Author     : Robert Jarzmik  <robert.jarzmik@free.fr>
-- Company    : 
-- Created    : 2016-12-06
-- Last update: 2017-02-18
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

library rjarzmik;
use rjarzmik.slv_utils.slv_is_x;

entity ALU_Set_Lower_Than is
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
end entity ALU_Set_Lower_Than;

architecture str of ALU_Set_Lower_Than is
begin  -- architecture str

  slt : process(clk, clkena, i_ra, i_rb)
  begin
    if clkena = '1' and rising_edge(clk) then
      if not slv_is_x(i_ra) and not slv_is_x(i_rb) and i_ra < i_rb then
        o_q <= to_unsigned(1, o_q'length);
      else
        o_q <= to_unsigned(0, o_q'length);
      end if;
    end if;
  end process slt;

end architecture str;
