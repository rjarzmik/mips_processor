-------------------------------------------------------------------------------
-- Title      : ALU set lower than
-- Project    : Source files in two directories, custom library name, VHDL'87
-------------------------------------------------------------------------------
-- File       : ALU_Set_Lower_Than.vhd
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

entity ALU_Set_Lower_Than is
  generic (
    DATA_WIDTH : integer
    );

  port (
    rst  : in  std_logic;
    i_ra : in  unsigned(DATA_WIDTH - 1 downto 0);
    i_rb : in  unsigned(DATA_WIDTH - 1 downto 0);
    o_q  : out unsigned(DATA_WIDTH * 2 - 1 downto 0)
    );
end entity ALU_Set_Lower_Than;

-------------------------------------------------------------------------------

architecture rtl of ALU_Set_Lower_Than is
  -----------------------------------------------------------------------------
  -- Internal signal declarations
  -----------------------------------------------------------------------------
  signal result : unsigned(DATA_WIDTH * 2 - 1 downto 0);

begin  -- architecture rtl

  o_q    <= result;
  result <= to_unsigned(0, DATA_WIDTH * 2) when rst = '1' else
            to_unsigned(1, DATA_WIDTH * 2) when i_ra < i_rb else
            to_unsigned(0, DATA_WIDTH * 2);

end architecture rtl;

-------------------------------------------------------------------------------
